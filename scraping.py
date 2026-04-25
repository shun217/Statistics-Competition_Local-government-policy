import requests
from bs4 import BeautifulSoup
import time
import random
import csv  # CSV操作用に追加

BASE_URL = "https://seijiyama.jp"
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
}

session = requests.Session()

def safe_get(url, params=None):
    print(f"[GET] {url}")
    res = session.get(url, headers=HEADERS, params=params)
    res.raise_for_status()
    time.sleep(random.uniform(1, 2))
    return res.text

def get_list_page(page):
    base_search_url = "https://seijiyama.jp/area/table/3624/BjtDe5/M"
    # 前回の回答で特定したパラメータ
    params = {
        "3734_252153_2": "2021",
        "3734_252153_1": "2012",
        "detect": "判定",
        "3734_252150_1": "",
        "S": "qipe2lcqbo",  # ※エラーが出る場合はここを最新に更新してください
        "_limit_3624": "200",
        "3734_252159_1": "1,2",
        "_page_3624": str(page)
    }
    return safe_get(base_search_url, params=params)

def extract_elections(html):
    soup = BeautifulSoup(html, "html.parser")
    elections = []
    rows = soup.select("tr.smp-row-data")
    for row in rows:
        tds = row.find_all("td")
        if len(tds) < 5: continue
        
        link_tag = tds[4].find("a")
        if link_tag and link_tag.get("href"):
            elections.append({
                "選挙名": tds[2].text.strip(),
                "投票日": tds[0].text.strip(),
                "都道府県": tds[3].text.strip(),
                "url": BASE_URL + link_tag.get("href")
            })
    return elections

def parse_detail(url):
    html = safe_get(url)
    soup = BeautifulSoup(html, "html.parser")
    winners = []
    rows = soup.select("tr.smp-row-data")
    for row in rows:
        tds = row.find_all("td")
        if len(tds) < 9: continue
        if tds[0].text.strip() == "当":
            winners.append({
                "氏名": tds[3].text.strip(),
                "得票数": tds[1].text.strip(),
                "年齢": tds[4].text.strip(),
                "性別": tds[5].text.strip(),
                "党派": tds[6].text.strip(),
                "新旧": tds[7].text.strip(),
                "肩書": tds[8].text.strip()
            })
    return winners

# =========================
# メイン処理：実行とCSV保存
# =========================
import os # ファイル存在確認用に追加

def run_and_save_csv():
    filename = "election_results_incremental.csv"
    fieldnames = ["選挙名", "投票日", "都道府県", "氏名", "得票数", "年齢", "性別", "党派", "新旧", "肩書"]
    
    # 1. 最初のみ：ファイルを作成してヘッダーを書き込む
    # すでにファイルがある場合は上書きしないよう注意（新規作成）
    with open(filename, mode="w", encoding="utf-8-sig", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()

    print("=== START INCREMENTAL SCRAPING (11 to 50 pages) ===")
    
    buffer = []  # データを一時的に溜めるバッファ
    total_count = 0

    for page in range(11, 51):  # 11ページ目からスタート
        print(f"\n--- {page}/50 ページ目を取得中 ---")
        try:
            html = get_list_page(page)
            elections = extract_elections(html)
            
            if not elections:
                break

            for i, e in enumerate(elections):
                print(f"  [{i+1}/{len(elections)}] 詳細取得: {e['選挙名']}")
                try:
                    winners = parse_detail(e["url"])
                    for w in winners:
                        row = {
                            "選挙名": e["選挙名"], "投票日": e["投票日"], 
                            "都道府県": e["都道府県"], **w
                        }
                        buffer.append(row)
                        total_count += 1

                        # ★ 100件溜まったらCSVに追記してバッファを空にする
                        if len(buffer) >= 100:
                            save_to_csv(filename, fieldnames, buffer)
                            print(f"  >> 【保存】累計 {total_count} 件をCSVに書き出しました。")
                            buffer = [] # バッファをリセット

                except Exception as ex:
                    print(f"  [Error] 詳細失敗: {ex}")

        except requests.exceptions.HTTPError as e:
            print(f"\n中断されました: {e}")
            break

    # 最後にバッファに残っているデータ（100件に満たなかった分）を書き出す
    if buffer:
        save_to_csv(filename, fieldnames, buffer)
        print(f"  >> 【完了】残りの {len(buffer)} 件を保存しました。")

    print(f"\n=== 全工程終了 合計: {total_count} 件 ===")

# --- 追記用の補助関数 ---
def save_to_csv(filename, fieldnames, data_list):
    """バッファ内のデータをCSVに追記(append)する"""
    with open(filename, mode="a", encoding="utf-8-sig", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writerows(data_list)

if __name__ == "__main__":
    run_and_save_csv()