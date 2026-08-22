rm(list = ls())
#dev.off()

library(plm)
library(lmtest)
library(sandwich)

# データの分析
# 全国データでの固定効果モデル(教育費シェアはロジット変換)
# 変数を変えながら頑健性をチェックする
# 2値での回帰も必要に応じて行う
# 地域ごと、都市抽出による同様の分析

#======================================
# 1.　データの読み込みと編集

# 1.1 読みこみ
panel_read <- read.csv("data/crosssection.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)


# 1.2 政令指定都市・特別区の上乗せ分を見るためのダミー（全国共通の効果との交互作用）
grp_flag <- list(政令市 = panel_read$政令指定都市, 特別区 = panel_read$特別区)

for (g in names(grp_flag)) {
  for (k in 1:3) {
    panel_read[[paste0("議員", k, "年_", g)]] <-
      as.integer(grp_flag[[g]] == 1 & panel_read$議員経過年度 == k)
    panel_read[[paste0("首長", k, "年_", g)]] <-
      as.integer(grp_flag[[g]] == 1 & panel_read$首長経過年度 == k)
  }

  panel_read[[paste0("議員選挙年_", g)]] <-
    as.integer(grp_flag[[g]] == 1 & panel_read$議員選挙年 == 1)
  panel_read[[paste0("首長選挙年_", g)]] <-
    as.integer(grp_flag[[g]] == 1 & panel_read$首長選挙年 == 1)
}

# 1.2.1 2017年度の県費負担教職員給与負担の指定都市への移譲
# 政令市の教育費だけが2017年度に約2倍（ln差 +0.73）になる会計上の断絶。
# 年度固定効果は全国共通ショックしか吸収しないため、統制しないと
# 政令市の交互作用項が水準シフトを拾う（首長選挙年_政令市は6倍に膨らむ）
panel_read$政令市_post2017 <- as.integer(panel_read$政令指定都市 == 1 & panel_read$年度 >= 2017)

# 1.2.2 11節で処置ダミーと交差させる変数。標本平均で中央化する
# 中央化しないと経過年度ダミーの係数が「その変数が0の自治体での効果」になり解釈できない
ix_vars <- c(pop = "ln総人口", child = "児童生徒割合", cost = "経常収支比率.市町村財政.")

for (v in ix_vars)
  panel_read[[paste0(v, "_中央化")]] <- panel_read[[v]] - mean(panel_read[[v]])

# 1.3 パネルベースライン
panel <- pdata.frame(panel_read, index = c("地域コード", "年度"))

# 1.4 全モデル共通の設定
# 標準誤差は自治体単位のクラスター頑健標準誤差
cl <- function(x) vcovHC(x, type = "HC1", cluster = "group")

# 議員・首長の特性（ベースラインモデルで共通で入れる統制変数）
attr_ctrl <- "議会高齢化ギャップ + 議会女性ギャップ + 議員新人割合 +
              首長X65歳以上割合 + 首長新人割合 + 首長女性割合"

# 被説明変数・処置変数・統制変数を差し替えて固定効果モデルを推定する
fe <- function(treat, y, controls, effect = "twoways", data = panel) {
  f <- as.formula(paste(y, "~", treat, "+", controls))
  plm(f, data = data, model = "within", effect = effect)
}

# 3パターン（議員のみ・首長のみ・両方）をまとめて推定する
fe3 <- function(y, controls, treat_c = "factor(議員経過年度)", treat_p = "factor(首長経過年度)",
                effect = "twoways", data = panel) {
  list(議員のみ = fe(treat_c, y, controls, effect, data),
       首長のみ = fe(treat_p, y, controls, effect, data),
       両方 = fe(paste(treat_c, "+", treat_p), y, controls, effect, data))
}

#=======================================

# 2. ベースライン固定効果モデルの適用（被説明変数は logit(教育費シェア)）

# 特別区ダミーは時間不変なので自治体固定効果に吸収される（plmが落とす）。
# 特別区の違いは8節の交互作用項で見る
ctrl_share <- paste(attr_ctrl, "+ 地域女性割合 + 経常収支比率.市町村財政. + 児童生徒割合 + 政令市_post2017")

# 2.1 議員経過年度のみ／首長経過年度のみ／両者を処置変数とする
base_share <- fe3("logit教育費シェア", ctrl_share)

lapply(base_share, summary)

# 2.2 選挙年ダミーにまとめてよいか、経過年度1,2,3の係数が等しいかを検定する
# 帰無仮説 b1 = b2 = b3 を制約行列で表し、クラスター頑健分散でWald検定する
wald_equal <- function(m, prefix, fixed = TRUE) {
  b <- coef(m)
  V <- cl(m)
  k <- grep(prefix, names(b), fixed = fixed)

  R <- matrix(0, length(k) - 1, length(b))
  for (j in seq_len(length(k) - 1)) {
    R[j, k[1]] <- 1
    R[j, k[j + 1]] <- -1
  }

  w <- as.numeric(t(R %*% b) %*% solve(R %*% V %*% t(R)) %*% (R %*% b))
  c(カイ二乗 = w, 自由度 = nrow(R), p値 = pchisq(w, nrow(R), lower.tail = FALSE))
}

round(wald_equal(base_share$両方, "factor(議員経過年度)"), 4)
round(wald_equal(base_share$両方, "factor(首長経過年度)"), 4)

# 2.3 両者を処置変数とする + ダミー
base_share$選挙年ダミー <- fe("議員選挙年 + 首長選挙年", "logit教育費シェア", ctrl_share)
summary(base_share$選挙年ダミー)

#=======================================

# 3. 結果を表にまとめる共通の道具

# 有意水準の星。*** 1%, ** 5%, * 10%
star <- function(p) as.character(symnum(p, c(0, .01, .05, .1, 1), c("***", "**", "*", "")))

# 「係数***(標準誤差)」の形にする
# trans = "share" は logit係数を教育費シェアの変化（%ポイント）に、
# trans = "pct"   は ln係数を教育費の変化率（%）に換算する
fmt <- function(m, trans = "none", p0 = NULL, pat = NULL) {
  ct <- coeftest(m, vcov = cl(m))
  est <- ct[, 1]
  se <- ct[, 2]

  # trans = "p" はp値を返す。patを与えるとそれに合う変数だけに絞る
  if (trans == "p") {
    k <- if (is.null(pat)) seq_len(nrow(ct)) else grep(pat, rownames(ct))
    p <- rep(NA_real_, nrow(ct))
    p[k] <- ct[k, 4]
    return(setNames(ifelse(is.na(p), "", sprintf("%.4f%s", p, star(p))), rownames(ct)))
  }

  if (trans == "share") {
    est <- (plogis(qlogis(p0) + ct[, 1]) - p0) * 100
    se <- ct[, 2] * p0 * (1 - p0) * 100   # デルタ法（dp/dlogit = p(1-p)）
  }

  if (trans == "pct") {
    est <- (exp(ct[, 1]) - 1) * 100
    se <- exp(ct[, 1]) * ct[, 2] * 100    # デルタ法
  }

  setNames(sprintf("%.4f%s (%.4f)", est, star(ct[, 4]), se), rownames(ct))
}

# モデルによって説明変数が違うので、全変数を行にそろえて空欄で埋める
tab_of <- function(models, trans = "none", p0 = NULL, pat = NULL) {
  vars <- unique(unlist(lapply(models, function(m) names(coef(m)))))
  x <- sapply(models, function(m) fmt(m, trans, p0, pat)[vars])
  rownames(x) <- vars
  x[is.na(x)] <- ""
  x
}

# モデルに関する統計量。F検定もクラスター頑健分散で行う
info_of <- function(models) {
  sapply(models, function(m) {
    s <- summary(m)
    w <- pwaldtest(m, test = "F", vcov = cl)
    c(固定効果 = if (m$args$effect == "twoways") "自治体・年度" else "自治体",
      標準誤差 = "自治体クラスター",
      観測数 = nobs(m),
      自治体数 = pdim(m)$nT$n,
      年度数 = pdim(m)$nT$T,
      決定係数 = sprintf("%.4f", s$r.squared["rsq"]),
      調整済み決定係数 = sprintf("%.4f", s$r.squared["adjrsq"]),
      F統計量 = sprintf("%.2f", w$statistic),
      F検定のp値 = format.pval(w$p.value, digits = 3))
  })
}

# Excelでそのまま開けるようBOM付きUTF-8で書き出す
write_bom <- function(x, path) {
  tmp <- tempfile()
  write.csv(x, tmp, fileEncoding = "UTF-8")
  con <- file(path, "wb")
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), readBin(tmp, "raw", file.size(tmp))), con)
  close(con)
}

# wild cluster bootstrap（少数クラスターで識別される係数の検定）
# within変換後のデータに対し、帰無仮説を課した残差をRademacher重みで撹乱する
wcb <- function(m, var, B = 199, seed = 1) {
  X <- model.matrix(m)
  y <- as.numeric(pmodel.response(m))
  g <- droplevels(index(m)[[1]])
  j <- which(colnames(X) == var)

  XtXi <- solve(crossprod(X))

  # クラスター頑健なt値
  tstat <- function(yy) {
    b <- XtXi %*% crossprod(X, yy)
    u <- as.numeric(yy - X %*% b)
    V <- XtXi %*% crossprod(rowsum(X * u, g)) %*% XtXi
    b[j] / sqrt(V[j, j])
  }

  t_obs <- tstat(y)

  # 帰無仮説（当該係数=0）を課した残差を作る
  Xr <- X[, -j, drop = FALSE]
  fit_r <- as.numeric(Xr %*% solve(crossprod(Xr), crossprod(Xr, y)))
  res_r <- y - fit_r

  set.seed(seed)
  t_boot <- replicate(B, {
    w <- sample(c(-1, 1), nlevels(g), TRUE)[g]
    tstat(fit_r + w * res_r)
  })

  c(t値 = t_obs, ブートストラップp値 = (1 + sum(abs(t_boot) >= abs(t_obs))) / (B + 1))
}

# p値の行にだけ、指定したモデルのブートストラップp値を右端の列として付ける
add_boot <- function(res, models, B = 199) {
  k <- which(res[, "指標"] == "p値")

  for (nm in names(models)) {
    m <- models[[nm]]
    b <- rep("", nrow(res))

    for (i in k) {
      v <- rownames(res)[i]
      if (v %in% names(coef(m))) {
        p <- wcb(m, v, B)[2]
        b[i] <- sprintf("%.4f%s", p, star(p))
      }
    }

    res <- cbind(res, b)
    colnames(res)[ncol(res)] <- sprintf("%s(ブートストラップp値)", nm)
  }

  res
}

# 係数・換算値・p値・モデル情報を縦に積んだ結果表。transは"share"（logit）か"pct"（ln）
res_table <- function(models, trans, p0 = NULL) {
  head <- if (trans == "share") "logit係数" else "ln係数"
  lab <- if (trans == "share") sprintf("教育費シェア%%pt (評価点 %.4f)", p0) else "教育費の変化率(%)"

  rbind(cbind(指標 = head, tab_of(models)),
        cbind(指標 = lab, tab_of(models, trans, p0)),
        cbind(指標 = "p値", tab_of(models, "p")),
        cbind(指標 = "モデル情報", info_of(models)))
}

# 交互作用モデルの結果表。少数クラスターで識別される係数にはブートストラップp値も付ける
group_table <- function(models, trans, p0 = NULL, boot = "両方") {
  add_boot(res_table(models, trans, p0), models[boot])
}

#=======================================

# 4. logit(教育費シェア)モデルの出力
# 換算はシェアの水準に依存するので、平均的な教育費シェアの点で評価する

p_bar <- mean(panel_read$教育費シェア)

result_share <- res_table(base_share, "share", p_bar)

result_share

write_bom(result_share, "analysis_results.csv")

#=======================================

# 5. 被説明変数を ln(教育費) とするモデル
# 規模の統制は児童生徒数（対数）。シェアと違い分母で規模を割っていないため

ctrl_ln <- paste(attr_ctrl, "+ 地域女性割合 + 経常収支比率.市町村財政. + ln児童生徒数 + 政令市_post2017")

# 5.1-5.3 議員経過年度のみ／首長経過年度のみ／両方
base_ln <- fe3("ln教育費.市町村財政.", ctrl_ln)

lapply(base_ln, summary)

# 5.4 出力（ln係数と、教育費の変化率(%)への換算）
result_ln <- res_table(base_ln, "pct")

result_ln

write_bom(result_ln, "analysis_results_lnedu.csv")


#=======================================

# 6. 政令指定都市ダミーとセクション2のモデルの合成
# 全国共通の効果に政令市の上乗せ分（交互作用）を加える。政令市ダミー自体は
# 時間不変なので自治体固定効果に吸収される


# 6.1 経過年度を処置変数とする
treat_c_sd <- paste("factor(議員経過年度) +", paste(sprintf("議員%d年_政令市", 1:3), collapse = " + "))
treat_p_sd <- paste("factor(首長経過年度) +", paste(sprintf("首長%d年_政令市", 1:3), collapse = " + "))

seirei_share <- fe3("logit教育費シェア", ctrl_share, treat_c_sd, treat_p_sd)

lapply(seirei_share, summary)

result_seirei <- group_table(seirei_share, "share", p_bar)

result_seirei
write_bom(result_seirei, "analysis_results_seirei.csv")

# 6.1.1 6.2で選挙年ダミーにまとめてよいか、経過年度1,2,3の係数が等しいかを検定する
# 全国共通分と政令市の上乗せ分の両方を確かめる
round(wald_equal(seirei_share$両方, "factor(議員経過年度)"), 4)
round(wald_equal(seirei_share$両方, "factor(首長経過年度)"), 4)

round(wald_equal(seirei_share$両方, "^議員[123]年_政令市$", fixed = FALSE), 4)
round(wald_equal(seirei_share$両方, "^首長[123]年_政令市$", fixed = FALSE), 4)

# 6.2 経過年度の代わりに選挙年ダミーを処置変数とする
treat_c_sy <- "議員選挙年 + 議員選挙年_政令市"
treat_p_sy <- "首長選挙年 + 首長選挙年_政令市"

seirei_year <- fe3("logit教育費シェア", ctrl_share, treat_c_sy, treat_p_sy)

lapply(seirei_year, summary)

result_seirei_year <- group_table(seirei_year, "share", p_bar, c("首長のみ", "両方"))

result_seirei_year
write_bom(result_seirei_year, "analysis_results_seirei_year.csv")

#=======================================

# 7. 係数の等値検定（wild cluster bootstrap版）
# 政令市は20市しかないため、解析的なクラスター頑健分散は信頼できない

# 7.1 等値検定（2.2や6.1.1のWald検定）のwild cluster bootstrap版
# 帰無仮説「係数が全て等しい」を課すと、3つのダミーは合計1本にまとまる

wcb_wald <- function(m, pat, fixed = TRUE, B = 199, seed = 1) {
  X <- model.matrix(m)
  y <- as.numeric(pmodel.response(m))
  g <- droplevels(index(m)[[1]])
  k <- grep(pat, colnames(X), fixed = fixed)

  R <- matrix(0, length(k) - 1, ncol(X))
  for (j in seq_len(length(k) - 1)) {
    R[j, k[1]] <- 1
    R[j, k[j + 1]] <- -1
  }

  XtXi <- solve(crossprod(X))

  wald <- function(yy) {
    b <- XtXi %*% crossprod(X, yy)
    u <- as.numeric(yy - X %*% b)
    V <- XtXi %*% crossprod(rowsum(X * u, g)) %*% XtXi
    as.numeric(t(R %*% b) %*% solve(R %*% V %*% t(R)) %*% (R %*% b))
  }

  w_obs <- wald(y)

  Xr <- cbind(X[, -k, drop = FALSE], rowSums(X[, k, drop = FALSE]))
  fit_r <- as.numeric(Xr %*% solve(crossprod(Xr), crossprod(Xr, y)))
  res_r <- y - fit_r

  set.seed(seed)
  w_boot <- replicate(B, {
    ww <- sample(c(-1, 1), nlevels(g), TRUE)[g]
    wald(fit_r + ww * res_r)
  })

  c(カイ二乗 = w_obs, ブートストラップp値 = (1 + sum(w_boot >= w_obs)) / (B + 1))
}

# 全国共通分と、指定したグループの上乗せ分の等値検定をまとめる
wald_targets <- function(g) {
  list(c("全国_議員", "factor(議員経過年度)", "TRUE"),
       c("全国_首長", "factor(首長経過年度)", "TRUE"),
       c(paste0(g, "_議員"), sprintf("^議員[123]年_%s$", g), "FALSE"),
       c(paste0(g, "_首長"), sprintf("^首長[123]年_%s$", g), "FALSE"))
}

wald_table <- function(m, targets) {
  x <- t(sapply(targets, function(z) {
    a <- wald_equal(m, z[2], fixed = as.logical(z[3]))
    b <- wcb_wald(m, z[2], fixed = as.logical(z[3]))
    c(カイ二乗 = sprintf("%.2f", a[1]),
      漸近p値 = sprintf("%.4f", a[3]),
      ブートストラップp値 = sprintf("%.4f", b[2]))
  }))
  rownames(x) <- sapply(targets, `[`, 1)
  x
}

result_wcb_wald <- wald_table(seirei_share$両方, wald_targets("政令市"))

result_wcb_wald


#=======================================

# 8. 6・7の特別区（東京23区）バージョン


# 8.1 経過年度を処置変数とする
treat_c_tk <- paste("factor(議員経過年度) +", paste(sprintf("議員%d年_特別区", 1:3), collapse = " + "))
treat_p_tk <- paste("factor(首長経過年度) +", paste(sprintf("首長%d年_特別区", 1:3), collapse = " + "))

tokubetsu_share <- fe3("logit教育費シェア", ctrl_share, treat_c_tk, treat_p_tk)

lapply(tokubetsu_share, summary)

result_tokubetsu <- group_table(tokubetsu_share, "share", p_bar)

result_tokubetsu
write_bom(result_tokubetsu, "analysis_results_tokubetsu.csv")

# 8.1.1 8.2で選挙年ダミーにまとめてよいか、経過年度1,2,3の係数が等しいかを検定する
result_wcb_wald_tk <- wald_table(tokubetsu_share$両方, wald_targets("特別区"))

result_wcb_wald_tk

result_wald <- rbind(cbind(グループ = "政令市", result_wcb_wald),
                     cbind(グループ = "特別区", result_wcb_wald_tk))

result_wald

write_bom(result_wald, "analysis_results_wald.csv")

# 8.2 経過年度の代わりに選挙年ダミーを処置変数とする
tokubetsu_year <- fe3("logit教育費シェア", ctrl_share,
                      "議員選挙年 + 議員選挙年_特別区", "首長選挙年 + 首長選挙年_特別区")

lapply(tokubetsu_year, summary)

result_tokubetsu_year <- group_table(tokubetsu_year, "share", p_bar, c("首長のみ", "両方"))

result_tokubetsu_year
write_bom(result_tokubetsu_year, "analysis_results_tokubetsu_year.csv")



#=======================================

# 9. 6-8の交互作用を、5のモデル（被説明変数 ln(教育費)）で検証する
# 統制変数はctrl_ln（規模の統制が児童生徒割合ではなくln児童生徒数）

# 9.1 政令市（経過年度／選挙年ダミー）
ln_seirei_share <- fe3("ln教育費.市町村財政.", ctrl_ln, treat_c_sd, treat_p_sd)
ln_seirei_year <- fe3("ln教育費.市町村財政.", ctrl_ln, treat_c_sy, treat_p_sy)

result_ln_seirei <- group_table(ln_seirei_share, "pct")
result_ln_seirei_year <- group_table(ln_seirei_year, "pct", boot = c("首長のみ", "両方"))

result_ln_seirei
result_ln_seirei_year

write_bom(result_ln_seirei, "analysis_results_ln_seirei.csv")
write_bom(result_ln_seirei_year, "analysis_results_ln_seirei_year.csv")

# 9.2 特別区（経過年度／選挙年ダミー）
ln_tokubetsu_share <- fe3("ln教育費.市町村財政.", ctrl_ln, treat_c_tk, treat_p_tk)
ln_tokubetsu_year <- fe3("ln教育費.市町村財政.", ctrl_ln,
                         "議員選挙年 + 議員選挙年_特別区", "首長選挙年 + 首長選挙年_特別区")

result_ln_tokubetsu <- group_table(ln_tokubetsu_share, "pct")
result_ln_tokubetsu_year <- group_table(ln_tokubetsu_year, "pct", boot = c("首長のみ", "両方"))

result_ln_tokubetsu
result_ln_tokubetsu_year

write_bom(result_ln_tokubetsu, "analysis_results_ln_tokubetsu.csv")
write_bom(result_ln_tokubetsu_year, "analysis_results_ln_tokubetsu_year.csv")

# 9.3 選挙年ダミーへ集約してよいかの等値検定（7.1・8.1.1と同じ形）
result_ln_wald <- rbind(cbind(グループ = "政令市", wald_table(ln_seirei_share$両方, wald_targets("政令市"))),
                        cbind(グループ = "特別区", wald_table(ln_tokubetsu_share$両方, wald_targets("特別区"))))

result_ln_wald

write_bom(result_ln_wald, "analysis_results_ln_wald.csv")

#=======================================

# 10. 政令市と人口規模の近い市との比較（6.2のモデルを規模の近い市に限定して推定）
#
# 政令指定は人口50万人が法律上の要件だが、実際の運用基準は70万人程度で
# 申請と閣議決定を要するため、50万人で処置が不連続にならない。
# 実際、人口50万人以上36自治体のうち政令市は20で、東大阪(50.1万)から
# 船橋(62.7万)まではすべて非指定。政令市の最小は静岡(70.3万)で、
# 62.7万と70.3万の間に観測が存在しない（完全分離）。
# したがって回帰不連続デザインは識別不能なので、規模を揃えた条件付き比較にする。

# 10.1 割当の状況を確認する（RDDが使えない根拠）
pop16 <- setNames(panel_read$総人口[panel_read$年度 == 2016],
                  panel_read$地域コード[panel_read$年度 == 2016])
sei16 <- setNames(panel_read$政令指定都市[panel_read$年度 == 2016],
                  panel_read$地域コード[panel_read$年度 == 2016])
tok16 <- setNames(panel_read$特別区[panel_read$年度 == 2016],
                  panel_read$地域コード[panel_read$年度 == 2016])

result_assign <- rbind(
  `政令市の人口下限` = round(min(pop16[sei16 == 1])),
  `非政令市(市)の人口上限` = round(max(pop16[sei16 == 0 & tok16 == 0])),
  `人口50万以上の自治体数` = sum(pop16 >= 5e5),
  `うち政令市` = sum(pop16 >= 5e5 & sei16 == 1),
  `人口50-70万の自治体数` = sum(pop16 >= 5e5 & pop16 < 7e5),
  `うち政令市` = sum(pop16 >= 5e5 & pop16 < 7e5 & sei16 == 1))

result_assign

write_bom(result_assign, "analysis_results_assign.csv")

# 10.2 特別区を除き、人口40万-150万の市に限定して6.2のモデルを推定する
near_cd <- names(pop16)[tok16 == 0 & pop16 >= 4e5 & pop16 <= 1.5e6]

panel_near <- pdata.frame(panel_read[panel_read$地域コード %in% near_cd, ],
                          index = c("地域コード", "年度"))

near_year <- fe3("logit教育費シェア", ctrl_share, treat_c_sy, treat_p_sy, data = panel_near)

lapply(near_year, summary)

result_near <- group_table(near_year, "share", p_bar, c("首長のみ", "両方"))

result_near

write_bom(result_near, "analysis_results_near.csv")

#=======================================

# 11. 選挙サイクルの効果が自治体の特性で異なるかの検証
# ベースライン（2節・5節）に、交差させる変数（1.2.2のix_vars）の主効果と
# 処置ダミーとの交差項を加える。変数は中央化してあるので、経過年度ダミーの係数は
# 平均的な自治体での効果、交差項はその変数が1増えるごとの上乗せ分を表す。
# 政令市・特別区ダミーと違い3変数とも時間変動するので自治体固定効果に吸収されない

ix_ys <- c(シェア = "logit(教育費シェア)", ln = "ln(教育費)")

# 11.1 交差項入りのモデル（議員のみ・首長のみ・両方 × 被説明変数2つ）
# 交差項の変数名は式に現れる順で付くので、中央化した変数を先に書いて3モデルで行名を揃える
ix_fit <- function(v) {
  cv <- paste0(v, "_中央化")
  treat_c <- paste(cv, "* factor(議員経過年度)")
  treat_p <- paste(cv, "* factor(首長経過年度)")

  # 生の変数が統制変数に入っている場合は中央化したものに差し替える（重複を避ける）
  list(シェア = fe3("logit教育費シェア", sub(v, cv, ctrl_share, fixed = TRUE), treat_c, treat_p),
       ln = fe3("ln教育費.市町村財政.", sub(v, cv, ctrl_ln, fixed = TRUE), treat_c, treat_p))
}

ix_fits <- lapply(ix_vars, ix_fit)

# 11.2 結果表（交差変数ごとに被説明変数2つ）
result_ix <- lapply(ix_fits, function(f)
  list(シェア = res_table(f$シェア, "share", p_bar), ln = res_table(f$ln, "pct")))

for (tag in names(ix_vars)) {
  write_bom(result_ix[[tag]]$シェア, sprintf("analysis_results_%s.csv", tag))
  write_bom(result_ix[[tag]]$ln, sprintf("analysis_results_ln_%s.csv", tag))
}

result_ix$pop$シェア
result_ix$child$シェア
result_ix$cost$シェア

# 11.3 交差項が全てゼロ（＝その変数による異質性がない）をクラスター頑健分散でWald検定する
wald_zero <- function(m, pat) {
  b <- coef(m)
  V <- cl(m)
  k <- grep(pat, names(b))

  w <- as.numeric(t(b[k]) %*% solve(V[k, k], b[k]))
  c(カイ二乗 = w, 自由度 = length(k), p値 = pchisq(w, length(k), lower.tail = FALSE))
}

# 交差変数 × 被説明変数 × 検定対象（議員／首長／両方）の総当たり
result_ix_wald <- do.call(rbind, lapply(names(ix_vars), function(tag) {
  cv <- paste0(ix_vars[[tag]], "_中央化")

  do.call(rbind, lapply(names(ix_ys), function(y) {
    x <- t(sapply(c(議員 = "議員", 首長 = "首長", `議員・首長` = ""),
                  function(z) round(wald_zero(ix_fits[[tag]][[y]]$両方,
                                              sprintf("^%s:factor\\(%s", cv, z)), 4)))
    cbind(交差変数 = ix_vars[[tag]], 被説明変数 = ix_ys[[y]], x)
  }))
}))

result_ix_wald

write_bom(result_ix_wald, "analysis_results_ix_wald.csv")

# 11.4 交差項の係数は変数1単位あたりなので、換算用に標準偏差を出しておく
result_ix_sd <- sapply(ix_vars, function(v) round(sd(panel_read[[v]]), 4))

result_ix_sd
