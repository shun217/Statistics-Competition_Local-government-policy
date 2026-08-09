#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
seijiyama.jp 地方選挙スクレイパー(全件・レジューム対応版)
============================================================

投票日 2022/01/01〜2022/03/31・選挙種別 1 と 2 の選挙を全ページ巡回し、
各選挙の候補者情報(当落含む)と選挙メタ情報を取得して CSV に保存する。

このバージョンで追加した機能:
  1) レジューム   … 中断しても再実行すれば途中(未処理の詳細ページ)から続行。
                     進捗は progress ファイルに逐次保存。CSV は追記。
  2) 進捗バー/ETA … tqdm があれば選挙単位のバーと残り時間を表示。
                     無ければ簡易パーセント表示に自動フォールバック。
  3) 失敗ログ     … 取得に失敗した一覧ページ・詳細ページを failures ログに記録。
                     最後に --retry-failures で再試行できる。

■ ページ構造(確認済み)
  一覧: <table id="smp-table-3624"> 内 tr.smp-row-data
        col-1 投票日 / col-2 告示日 / col-3 選挙名 / col-4 都道府県 /
        col-5 <a href="/area/card/3624/XXXX/M?..."> 候補者ページ
        ページ送り: &_page_3624=N (全37ページ)
  詳細: div.whole(選挙メタ) + table.smp-table(候補者)
        候補者行 tr.smp-row-data:
          col-1 当落(「当」で当選) / col-2 得票数 / col-4 氏名(a.name-kanji) /
          col-5 年齢 / col-6 性別 / col-7 党派 / col-8 新旧 / col-9 主な肩書き

使い方:
    pip install requests beautifulsoup4 lxml tqdm     # tqdm は任意
    python scrape_seijiyama_full.py                    # 通常実行 / 再実行で自動再開
    python scrape_seijiyama_full.py --retry-failures   # 失敗分だけ再試行
    python scrape_seijiyama_full.py --restart          # 進捗を捨てて最初からやり直し

出力(1候補者=1行):
    seijiyama_result_2022.csv       … 本体(追記)
    seijiyama_progress_2022.json    … 進捗(処理済み詳細URL・処理済みページ)
    seijiyama_failures_2022.log     … 失敗した URL の記録
"""

import argparse
import csv
import json
import os
import re
import sys
import time
from urllib.parse import urljoin, urlparse, parse_qs, urlencode, urlunparse

import requests
from bs4 import BeautifulSoup

# 進捗バー(無くても動く)
try:
    from tqdm import tqdm
    HAS_TQDM = True
except Exception:
    HAS_TQDM = False

# ---------------------------------------------------------------------------
# 設定
# ---------------------------------------------------------------------------
BASE = "https://seijiyama.jp"

# 検索条件:
#   3734_252153_1 / _2 … 投票日の範囲(2022/01/01〜2022/03/31)
#   3734_252159_1       … 選挙種別。1 と 2 の両方を指定(重複キーで OR 条件)
#   smp_sf_button_3734  … 検索ボタン。これを付けた URL が検索実行のエントリ。
#                          ページ送りはこの URL ではなくページャの実リンク
#                          (get_pager_template)を手本に組み立てること。
LIST_URL = (
    "https://seijiyama.jp/area/table/3624/BjtDe5/M"
    "?detect=%E5%88%A4%E5%AE%9A"
    "&S=qipe2lcqbo"
    "&_sort_length=3"
    "&_limit_3624=200"
    "&3734_252153_1=2022%2F01%2F01"
    "&3734_252153_2=2022%2F03%2F31"
    "&3734_252159_1=1"
    "&3734_252159_1=2"
    "&3734_252150_1="
    "&smp_sf_button_3734=%E6%A4%9C%E7%B4%A2"
)

PAGE_PARAM = "_page_3624"
PER_PAGE = 200
SLEEP_LIST = 1.5
SLEEP_DETAIL = 1.0
TIMEOUT = 30

OUTPUT = "seijiyama_result_2022.csv"
PROGRESS = "seijiyama_progress_2022.json"
FAILLOG = "seijiyama_failures_2022.log"

FIELDNAMES = [
    "投票日", "告示日", "選挙名", "都道府県", "detail_url",
    "定数", "候補者数", "執行理由", "有権者数", "投票率", "前回投票率",
    "得票数", "氏名", "年齢", "性別", "党派", "新旧", "主な肩書き", "elected",
]

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    ),
    "Accept-Language": "ja,en;q=0.9",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
}

session = requests.Session()
session.headers.update(HEADERS)


# ---------------------------------------------------------------------------
# 進捗ファイルの読み書き
# ---------------------------------------------------------------------------
def load_progress():
    """
    進捗を読み込む。
    構造: {"done_details": [url,...], "done_pages": [n,...], "total_pages": int|None}
    """
    if os.path.exists(PROGRESS):
        try:
            with open(PROGRESS, encoding="utf-8") as f:
                d = json.load(f)
            d.setdefault("done_details", [])
            d.setdefault("done_pages", [])
            d.setdefault("total_pages", None)
            return d
        except Exception:
            pass
    return {"done_details": [], "done_pages": [], "total_pages": None}


def save_progress(prog):
    tmp = PROGRESS + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(prog, f, ensure_ascii=False)
    os.replace(tmp, PROGRESS)   # 原子的に置換(途中で壊れない)


def log_failure(kind, url, reason=""):
    with open(FAILLOG, "a", encoding="utf-8") as f:
        f.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')}\t{kind}\t{url}\t{reason}\n")


# ---------------------------------------------------------------------------
# 汎用ユーティリティ
# ---------------------------------------------------------------------------
def get(url, tries=3, kind="page"):
    for i in range(tries):
        try:
            r = session.get(url, timeout=TIMEOUT)
            if r.status_code == 200:
                r.encoding = r.apparent_encoding or r.encoding or "utf-8"
                return r.text
            reason = f"HTTP {r.status_code}"
        except requests.RequestException as e:
            reason = str(e)
        time.sleep(2 * (i + 1))
    log_failure(kind, url, reason)
    return None


def norm(text):
    if text is None:
        return ""
    return re.sub(r"\s+", " ", text.replace("\u3000", " ")).strip()


def get_pager_template(soup):
    """
    ページャ(table.smp-pager)内の <a> のうち _page_3624 を持つものを1つ、
    「手本URL」として返す。この手本URLは検索ボタン(smp_sf_button_3734)を
    含まないページ送り専用の形式なので、これを基に任意ページのURLを合成すると
    1ページ目に戻される問題が起きない。見つからなければ None。
    """
    for a in soup.select("table.smp-pager a[href]"):
        href = a.get("href", "")
        q = parse_qs(urlparse(href).query)
        if "_page_3624" in q:
            return urljoin(BASE, href)
    return None


def pager_max_page(soup):
    """ページャの <a> から最大ページ番号を返す(取れなければ None)。"""
    mx = None
    for a in soup.select("table.smp-pager a[href]"):
        q = parse_qs(urlparse(a.get("href", "")).query)
        v = q.get("_page_3624", [None])[0]
        if v and v.isdigit():
            n = int(v)
            mx = n if mx is None else max(mx, n)
    return mx


def build_page_url(template_url, page):
    """
    手本URL(ページャ由来)の _page_3624 を指定ページに差し替えて返す。
    元の検索条件パラメータ(S, _limit_3624, 日付, 種別など)はそのまま保持される。
    """
    parts = urlparse(template_url)
    q = parse_qs(parts.query, keep_blank_values=True)
    q[PAGE_PARAM] = [str(page)]
    return urlunparse(parts._replace(query=urlencode(q, doseq=True)))


def cell_by_col(tr, col_num):
    for td in tr.find_all("td"):
        if f"smp-cell-col-{col_num}" in (td.get("class") or []):
            return td
    return None


# ---------------------------------------------------------------------------
# 一覧ページ
# ---------------------------------------------------------------------------
def parse_total_pages(soup):
    count_tag = soup.find("font", class_="smp-count")
    if count_tag:
        m = re.search(r"[\d,]+", count_tag.get_text())
        if m:
            total = int(m.group(0).replace(",", ""))
            return total, (total + PER_PAGE - 1) // PER_PAGE
    max_page = None
    for a in soup.select("table.smp-pager a"):
        t = norm(a.get_text())
        if t.isdigit():
            n = int(t)
            max_page = n if max_page is None else max(max_page, n)
    return None, max_page


def parse_list_rows(html):
    soup = BeautifulSoup(html, "lxml")
    table = soup.find("table", id="smp-table-3624")
    if table is None:
        for t in soup.find_all("table"):
            head = norm(t.get_text(" "))
            if "投票日" in head and "選挙名" in head:
                table = t
                break
    if table is None:
        return [], soup

    rows = []
    for tr in table.find_all("tr"):
        if "smp-row-data" not in (tr.get("class") or []):
            continue
        td_vote = cell_by_col(tr, 1)
        td_notice = cell_by_col(tr, 2)
        td_name = cell_by_col(tr, 3)
        td_pref = cell_by_col(tr, 4)
        td_detail = cell_by_col(tr, 5)

        detail_url = ""
        if td_detail is not None:
            a = td_detail.find("a", href=True)
            if a and a.get("href"):
                detail_url = urljoin(BASE, a["href"])
        if not detail_url:
            a = tr.find("a", href=True)
            if a:
                detail_url = urljoin(BASE, a["href"])

        vote = norm(td_vote.get_text()) if td_vote else ""
        name = norm(td_name.get_text()) if td_name else ""
        if not vote and not name and not detail_url:
            continue

        rows.append({
            "投票日": vote,
            "告示日": norm(td_notice.get_text()) if td_notice else "",
            "選挙名": name,
            "都道府県": norm(td_pref.get_text()) if td_pref else "",
            "detail_url": detail_url,
        })
    return rows, soup


# ---------------------------------------------------------------------------
# 詳細(候補者)ページ
# ---------------------------------------------------------------------------
def parse_election_meta(soup):
    meta = {"定数": "", "候補者数": "", "執行理由": "",
            "有権者数": "", "投票率": "", "前回投票率": ""}
    whole = soup.find("div", class_="whole")
    if whole is None:
        return meta
    for td in whole.find_all("td"):
        label_div = td.find("div")
        if not label_div:
            continue
        label = norm(label_div.get_text())
        value = norm(td.get_text().replace(label_div.get_text(), "", 1))
        if "定数" in label and "候補者" in label:
            m = re.findall(r"\d+", value)
            if len(m) >= 2:
                meta["定数"], meta["候補者数"] = m[0], m[1]
            elif len(m) == 1:
                meta["定数"] = m[0]
        elif "定数" in label:
            meta["定数"] = value
        elif "候補者" in label:
            meta["候補者数"] = value
        elif "執行理由" in label:
            meta["執行理由"] = value
        elif "有権者" in label:
            meta["有権者数"] = value.replace("人", "").strip()
        elif "前回投票率" in label:
            meta["前回投票率"] = value
        elif "投票率" in label:
            meta["投票率"] = value
    return meta


def find_candidate_table(soup):
    for table in soup.find_all("table", class_="smp-table"):
        tid = table.get("id", "")
        head = norm(table.get_text(" "))
        if "得票" in head and "氏名" in head and tid != "smp-table-3624":
            return table
    for table in soup.find_all("table"):
        head = norm(table.get_text(" "))
        if "得票" in head and "氏名" in head:
            return table
    return None


def parse_candidates(html):
    soup = BeautifulSoup(html, "lxml")
    meta = parse_election_meta(soup)
    table = find_candidate_table(soup)
    if table is None:
        return [], meta

    candidates = []
    for tr in table.find_all("tr"):
        if "smp-row-data" not in (tr.get("class") or []):
            continue
        td_flag = cell_by_col(tr, 1)
        td_votes = cell_by_col(tr, 2)
        td_name = cell_by_col(tr, 4)
        td_age = cell_by_col(tr, 5)
        td_sex = cell_by_col(tr, 6)
        td_party = cell_by_col(tr, 7)
        td_status = cell_by_col(tr, 8)
        td_title = cell_by_col(tr, 9)

        name = ""
        if td_name is not None:
            a = td_name.find("a", class_="name-kanji")
            name = norm(a.get_text()) if a else norm(td_name.get_text())

        votes = norm(td_votes.get_text()) if td_votes else ""
        flag = norm(td_flag.get_text()) if td_flag else ""
        elected = (flag == "当")
        if not name and not votes:
            continue

        candidates.append({
            "得票数": votes,
            "氏名": name,
            "年齢": norm(td_age.get_text()) if td_age else "",
            "性別": norm(td_sex.get_text()) if td_sex else "",
            "党派": norm(td_party.get_text()) if td_party else "",
            "新旧": norm(td_status.get_text()) if td_status else "",
            "主な肩書き": norm(td_title.get_text()) if td_title else "",
            "elected": elected,
        })
    return candidates, meta


# ---------------------------------------------------------------------------
# CSV ライター(追記・ヘッダ自動)
# ---------------------------------------------------------------------------
def open_writer():
    """CSV を追記モードで開く。新規なら BOM 付きでヘッダを書く。"""
    is_new = (not os.path.exists(OUTPUT)) or os.path.getsize(OUTPUT) == 0
    f = open(OUTPUT, "a", newline="", encoding="utf-8-sig")
    writer = csv.DictWriter(f, fieldnames=FIELDNAMES)
    if is_new:
        writer.writeheader()
    return f, writer


def write_election(writer, row, candidates, meta):
    base = {**row, **{k: meta.get(k, "") for k in
            ["定数", "候補者数", "執行理由", "有権者数", "投票率", "前回投票率"]}}
    n = 0
    if candidates:
        for c in candidates:
            writer.writerow({**base, **c})
            n += 1
    else:
        writer.writerow(base)
        n += 1
    return n


# ---------------------------------------------------------------------------
# 進捗表示ヘルパ
# ---------------------------------------------------------------------------
class Progress:
    """tqdm があればバー、無ければ簡易表示。"""
    def __init__(self, total, desc):
        self.total = total
        self.n = 0
        self.start = time.time()
        if HAS_TQDM:
            self.bar = tqdm(total=total, desc=desc, unit="件")
        else:
            self.bar = None
            print(f"{desc}: 全 {total} 件")

    def update(self, k=1):
        self.n += k
        if self.bar:
            self.bar.update(k)
        else:
            if self.n % 20 == 0 or self.n == self.total:
                elapsed = time.time() - self.start
                rate = self.n / elapsed if elapsed > 0 else 0
                remain = (self.total - self.n) / rate if rate > 0 else 0
                print(f"  {self.n}/{self.total} "
                      f"({100*self.n/self.total:.1f}%) "
                      f"残り約 {remain/60:.1f} 分", flush=True)

    def close(self):
        if self.bar:
            self.bar.close()


# ---------------------------------------------------------------------------
# メイン: 全件巡回
# ---------------------------------------------------------------------------
def crawl_all():
    prog = load_progress()
    done_details = set(prog["done_details"])
    done_pages = set(prog["done_pages"])

    # まず1ページ目を取得して総ページ数を確定(未確定なら)
    first_html = get(LIST_URL, kind="list")
    if first_html is None:
        print("1ページ目の取得に失敗しました。ネットワーク/URLを確認してください。", file=sys.stderr)
        return
    _, soup0 = parse_list_rows(first_html)
    total, pages = parse_total_pages(soup0)

    # ページ送りの「手本URL」をページャの実リンクから取得する。
    # これは検索ボタンを含まないページ送り専用形式なので、_page_3624 を
    # 差し替えるだけで任意ページへ正しく飛べる(1ページ目に戻されない)。
    page_template = get_pager_template(soup0)
    pager_mx = pager_max_page(soup0)
    if pager_mx:
        pages = max(pages or 0, pager_mx)

    if pages:
        prog["total_pages"] = pages
        print(f"総件数 {total} 件 / {pages} ページ")
    else:
        pages = prog["total_pages"] or 37
        print(f"総ページ数を検出できず。{pages} ページと仮定して続行。")

    if page_template is None and pages > 1:
        print("警告: ページャの手本URLを取得できませんでした。"
              "1ページ目のみ処理します。サイト構造の変更が疑われます。",
              file=sys.stderr)
        pages = 1
    save_progress(prog)

    # 全選挙(=詳細URL)をまず集める。
    all_elections = []          # list of row dict

    print("\n[フェーズ1] 一覧ページから全選挙リンクを収集")
    p_bar = Progress(pages, "一覧ページ")
    for page in range(1, pages + 1):
        if page == 1:
            html = first_html
            soup = soup0
        else:
            url = build_page_url(page_template, page)
            html = get(url, kind="list")
            time.sleep(SLEEP_LIST)
            soup = None
        if html is None:
            print(f"  page {page} 取得失敗(ログ記録済み)。スキップ。", file=sys.stderr)
            p_bar.update()
            continue
        rows, soup_parsed = parse_list_rows(html)
        if soup is None:
            soup = soup_parsed

        # 各ページのページャからも手本URLを拾い直しておく(万一形式が変わっても追随)
        tpl2 = get_pager_template(soup)
        if tpl2:
            page_template = tpl2

        all_elections.extend(rows)
        done_pages.add(page)
        prog["done_pages"] = sorted(done_pages)
        save_progress(prog)
        p_bar.update()
    p_bar.close()

    # 重複詳細URLを除去(順序維持)
    seen = set()
    uniq = []
    for r in all_elections:
        u = r["detail_url"]
        if u and u not in seen:
            seen.add(u)
            uniq.append(r)
    print(f"収集した選挙(ユニーク詳細URL): {len(uniq)} 件")

    # 未処理だけ残す
    todo = [r for r in uniq if r["detail_url"] not in done_details]
    print(f"未処理: {len(todo)} 件 / 処理済み: {len(done_details)} 件")

    if not todo:
        print("すべて処理済みです。完了。")
        return

    # フェーズ2: 詳細ページを巡回して CSV 追記
    print("\n[フェーズ2] 詳細ページを取得して候補者を記録")
    f, writer = open_writer()
    written = 0
    d_bar = Progress(len(todo), "詳細ページ")
    try:
        for r in todo:
            durl = r["detail_url"]
            dhtml = get(durl, kind="detail")
            if dhtml is not None:
                candidates, meta = parse_candidates(dhtml)
                written += write_election(writer, r, candidates, meta)
                f.flush()
                done_details.add(durl)
                # 進捗はこまめに保存(重いのでN件ごと)
                if len(done_details) % 10 == 0:
                    prog["done_details"] = list(done_details)
                    save_progress(prog)
            time.sleep(SLEEP_DETAIL)
            d_bar.update()
    finally:
        d_bar.close()
        prog["done_details"] = list(done_details)
        save_progress(prog)
        f.close()

    print(f"\n完了: {OUTPUT} に今回 {written} 行を追記(累計処理 {len(done_details)} 選挙)。")
    if os.path.exists(FAILLOG):
        print(f"失敗があれば {FAILLOG} を確認し、--retry-failures で再試行できます。")


# ---------------------------------------------------------------------------
# 失敗分の再試行
# ---------------------------------------------------------------------------
def retry_failures():
    if not os.path.exists(FAILLOG):
        print("失敗ログがありません。")
        return
    # 失敗ログから detail URL を抽出(重複除去)
    detail_urls = []
    seen = set()
    with open(FAILLOG, encoding="utf-8") as f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) >= 3 and parts[1] == "detail":
                u = parts[2]
                if u not in seen:
                    seen.add(u)
                    detail_urls.append(u)
    if not detail_urls:
        print("再試行対象(detail)がありません。")
        return

    prog = load_progress()
    done_details = set(prog["done_details"])
    detail_urls = [u for u in detail_urls if u not in done_details]
    print(f"再試行する詳細ページ: {len(detail_urls)} 件")

    # 失敗ログはリネームして退避(新たな失敗を別途記録するため)
    os.replace(FAILLOG, FAILLOG + ".retried")

    f, writer = open_writer()
    d_bar = Progress(len(detail_urls), "再試行")
    written = 0
    try:
        for durl in detail_urls:
            dhtml = get(durl, kind="detail")
            if dhtml is not None:
                candidates, meta = parse_candidates(dhtml)
                # 一覧情報が無いので detail_url だけ埋めて記録
                row = {"投票日": "", "告示日": "", "選挙名": "",
                       "都道府県": "", "detail_url": durl}
                written += write_election(writer, row, candidates, meta)
                f.flush()
                done_details.add(durl)
                prog["done_details"] = list(done_details)
                save_progress(prog)
            time.sleep(SLEEP_DETAIL)
            d_bar.update()
    finally:
        d_bar.close()
        f.close()
    print(f"再試行完了: {written} 行を追記。")


# ---------------------------------------------------------------------------
# エントリポイント
# ---------------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser(description="seijiyama 全件スクレイパー(レジューム対応)")
    ap.add_argument("--restart", action="store_true",
                    help="進捗と出力を捨てて最初からやり直す")
    ap.add_argument("--retry-failures", action="store_true",
                    help="失敗ログにある詳細ページだけ再試行する")
    args = ap.parse_args()

    if args.restart:
        for p in (OUTPUT, PROGRESS, FAILLOG):
            if os.path.exists(p):
                os.remove(p)
        print("進捗・出力をリセットしました。最初から取得します。")

    if args.retry_failures:
        retry_failures()
    else:
        crawl_all()


if __name__ == "__main__":
    main()
