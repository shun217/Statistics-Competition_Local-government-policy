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

# 1.2 地域ラベルの水準順を北から南に揃え、地域と年度の合成変数を作る
region_lv <- c("北海道", "東北", "関東", "中部", "近畿", "中国", "四国", "九州")

panel_read$地域 <- factor(panel_read$地域, levels = region_lv)
panel_read$地域年度 <- paste(panel_read$地域, panel_read$年度, sep = "_")

# 1.3 地域ごとの経過年度ダミー（どの地域でも選挙年=0が基準）
for (r in region_lv) {
  for (k in 1:3) {
    panel_read[[paste0("議員", k, "年_", r)]] <-
      as.integer(panel_read$地域 == r & panel_read$議員経過年度 == k)
    panel_read[[paste0("首長", k, "年_", r)]] <-
      as.integer(panel_read$地域 == r & panel_read$首長経過年度 == k)
  }

  # 地域ごとの選挙年ダミー（基準は選挙年以外＝経過年度1-3をまとめたもの）
  panel_read[[paste0("議員選挙年_", r)]] <-
    as.integer(panel_read$地域 == r & panel_read$議員選挙年 == 1)
  panel_read[[paste0("首長選挙年_", r)]] <-
    as.integer(panel_read$地域 == r & panel_read$首長選挙年 == 1)
}

# 1.4 政令指定都市・特別区の上乗せ分を見るためのダミー（全国共通の効果との交互作用）
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

# 1.5 パネルベースライン
panel <- pdata.frame(panel_read, index = c("地域コード", "年度"))

# 1.6 全モデル共通の設定
# 標準誤差は自治体単位のクラスター頑健標準誤差
cl <- function(x) vcovHC(x, type = "HC1", cluster = "group")

# 議員・首長の特性（どのモデルでも共通で入れる統制変数）
attr_ctrl <- "議会高齢化ギャップ + 議会女性ギャップ + 議員新人割合 +
              首長X65歳以上割合 + 首長新人割合 + 首長女性割合"

# 被説明変数・処置変数・統制変数を差し替えて固定効果モデルを推定する
fe <- function(treat, y, controls, effect = "twoways") {
  f <- as.formula(paste(y, "~", treat, "+", controls))
  plm(f, data = panel, model = "within", effect = effect)
}

# 3パターン（議員のみ・首長のみ・両方）をまとめて推定する
fe3 <- function(y, controls, treat_c = "factor(議員経過年度)", treat_p = "factor(首長経過年度)",
                effect = "twoways") {
  list(議員のみ = fe(treat_c, y, controls, effect),
       首長のみ = fe(treat_p, y, controls, effect),
       両方 = fe(paste(treat_c, "+", treat_p), y, controls, effect))
}

#=======================================

# 2. ベースライン固定効果モデルの適用（被説明変数は logit(教育費シェア)）

ctrl_share <- paste(attr_ctrl, "+ 地域女性割合 + 経常収支比率.市町村財政. + 児童生徒割合")

# 2.1-2.3 議員経過年度のみ／首長経過年度のみ／両者を処置変数とする
base_share <- fe3("logit教育費シェア", ctrl_share)

lapply(base_share, summary)

# 2.4.0 選挙年ダミーにまとめてよいか、経過年度1,2,3の係数が等しいかを検定する
# 帰無仮説 b1 = b2 = b3 を制約行列で表し、クラスター頑健分散でWald検定する
wald_equal <- function(m, prefix) {
  b <- coef(m)
  V <- cl(m)
  k <- grep(prefix, names(b), fixed = TRUE)

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

# 2.4 両者を処置変数とする + ダミー
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

  # trans = "p" は補正前のp値、"holm" はpatに合う処置変数を1つのfamilyとしたHolm補正p値
  if (trans %in% c("p", "holm")) {
    k <- if (is.null(pat)) seq_len(nrow(ct)) else grep(pat, rownames(ct))
    p <- rep(NA_real_, nrow(ct))
    p[k] <- if (trans == "holm") p.adjust(ct[k, 4], method = "holm") else ct[k, 4]
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
      地域年度ダミー = if (any(grepl("地域年度", names(coef(m))))) "あり" else "なし",
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

#=======================================

# 4. logit(教育費シェア)モデルの出力
# 換算はシェアの水準に依存するので、平均的な教育費シェアの点で評価する

p_bar <- mean(panel_read$教育費シェア)

result_share <- rbind(
  cbind(指標 = "logit係数", tab_of(base_share)),
  cbind(指標 = sprintf("教育費シェア%%pt (評価点 %.4f)", p_bar), tab_of(base_share, "share", p_bar)),
  cbind(指標 = "補正前p値", tab_of(base_share, "p", pat = "経過年度|選挙年")),
  cbind(指標 = "Holm補正p値", tab_of(base_share, "holm", pat = "経過年度|選挙年")),
  cbind(指標 = "モデル情報", info_of(base_share)))

result_share

write.csv(result_share, "analysis_results.csv", fileEncoding = "UTF-8")

#=======================================

# 5. 被説明変数を ln(教育費) とするモデル
# 規模の統制は児童生徒数（対数）。シェアと違い分母で規模を割っていないため

ctrl_ln <- paste(attr_ctrl, "+ 地域女性割合 + 経常収支比率.市町村財政. + ln児童生徒数")

# 5.1-5.3 議員経過年度のみ／首長経過年度のみ／両方
base_ln <- fe3("ln教育費.市町村財政.", ctrl_ln)

lapply(base_ln, summary)

# 5.4 出力（ln係数と、教育費の変化率(%)への換算）
result_ln <- rbind(
  cbind(指標 = "ln係数", tab_of(base_ln)),
  cbind(指標 = "教育費の変化率(%)", tab_of(base_ln, "pct")),
  cbind(指標 = "補正前p値", tab_of(base_ln, "p", pat = "経過年度")),
  cbind(指標 = "Holm補正p値", tab_of(base_ln, "holm", pat = "経過年度")),
  cbind(指標 = "モデル情報", info_of(base_ln)))

result_ln

write.csv(result_ln, "analysis_results_lnedu.csv", fileEncoding = "UTF-8")

#=======================================

# 6. 地域ごとの処置効果を見るモデル（2のモデルを流用）
# 年度効果は地域×年度の合成変数に置き換えるので effect は individual にする

# 6.1 地域ごとの経過年度ダミーを処置変数にする
treat_c_r <- paste(sprintf("議員%d年_%s", rep(1:3, times = length(region_lv)),
                           rep(region_lv, each = 3)), collapse = " + ")
treat_p_r <- paste(sprintf("首長%d年_%s", rep(1:3, times = length(region_lv)),
                           rep(region_lv, each = 3)), collapse = " + ")

# 6.2 地域×年度を固定効果として入れる（共通の年度効果はこれに吸収される）
ctrl_region <- paste(ctrl_share, "+ factor(地域年度)")

# 6.3 議員のみ／首長のみ／両方の3パターン
region_share <- fe3("logit教育費シェア", ctrl_region, treat_c_r, treat_p_r, effect = "individual")

lapply(region_share, summary)

# 6.4 出力（地域年度ダミーは行数が多いので表からは除く）
tab_r <- tab_of(region_share)
tab_r <- tab_r[grep("factor(地域年度)", rownames(tab_r), fixed = TRUE, invert = TRUE), ]

tab_r_pp <- tab_of(region_share, "share", p_bar)[rownames(tab_r), ]

# 地域ごとの処置ダミー全体を1つのfamilyとしてHolm補正する
treat_pat_r <- "^(議員|首長)[123]年_"

tab_r_p <- tab_of(region_share, "p", pat = treat_pat_r)[rownames(tab_r), ]
tab_r_holm <- tab_of(region_share, "holm", pat = treat_pat_r)[rownames(tab_r), ]

result_region <- rbind(
  cbind(指標 = "logit係数", tab_r),
  cbind(指標 = sprintf("教育費シェア%%pt (評価点 %.4f)", p_bar), tab_r_pp),
  cbind(指標 = "補正前p値", tab_r_p),
  cbind(指標 = "Holm補正p値", tab_r_holm),
  cbind(指標 = "モデル情報", info_of(region_share)))

result_region

write.csv(result_region, "analysis_results_region.csv", fileEncoding = "UTF-8")


#=======================================

# 8. 地域ごとの選挙年ダミーを処置変数とし、年度は通常の年度ダミーで扱う
# 6との違いは、経過年度1-3をまとめて選挙年ダミーにした点と、年次ショックを全国共通とみなす点

# 8.1 地域ごとの選挙年ダミーを処置変数にする（基準は選挙年以外）
treat_c_ry <- paste(sprintf("議員選挙年_%s", region_lv), collapse = " + ")
treat_p_ry <- paste(sprintf("首長選挙年_%s", region_lv), collapse = " + ")

treat_pat_ry <- "^(議員|首長)選挙年_"

# 8.2-8.4 議員のみ／首長のみ／両方
region_year <- fe3("logit教育費シェア", ctrl_share, treat_c_ry, treat_p_ry, effect = "twoways")

lapply(region_year, summary)

# 8.5 出力
result_region_year <- rbind(
  cbind(指標 = "logit係数", tab_of(region_year)),
  cbind(指標 = sprintf("教育費シェア%%pt (評価点 %.4f)", p_bar), tab_of(region_year, "share", p_bar)),
  cbind(指標 = "補正前p値", tab_of(region_year, "p", pat = treat_pat_ry)),
  cbind(指標 = "Holm補正p値", tab_of(region_year, "holm", pat = treat_pat_ry)),
  cbind(指標 = "モデル情報", info_of(region_year)))

result_region_year

write.csv(result_region_year, "analysis_results_region_year.csv", fileEncoding = "UTF-8")

#=======================================

# 9. 8と同じ選挙年ダミーを、地域×年度固定効果のもとで推定する
# 8との違いは年次ショックを地域ごとに許す点だけ。北海道の結果が残るかで
# 真の処置効果か地域固有の年次ショックかをある程度切り分ける

# 9.1-9.3 議員のみ／首長のみ／両方
region_year_rf <- fe3("logit教育費シェア", ctrl_region, treat_c_ry, treat_p_ry, effect = "individual")

lapply(region_year_rf, summary)

# 9.4 出力（地域年度ダミーは行数が多いので表からは除く）
tab_rf <- tab_of(region_year_rf)
tab_rf <- tab_rf[grep("factor(地域年度)", rownames(tab_rf), fixed = TRUE, invert = TRUE), ]

result_region_year_rf <- rbind(
  cbind(指標 = "logit係数", tab_rf),
  cbind(指標 = sprintf("教育費シェア%%pt (評価点 %.4f)", p_bar),
        tab_of(region_year_rf, "share", p_bar)[rownames(tab_rf), ]),
  cbind(指標 = "補正前p値", tab_of(region_year_rf, "p", pat = treat_pat_ry)[rownames(tab_rf), ]),
  cbind(指標 = "Holm補正p値", tab_of(region_year_rf, "holm", pat = treat_pat_ry)[rownames(tab_rf), ]),
  cbind(指標 = "モデル情報", info_of(region_year_rf)))

result_region_year_rf

write.csv(result_region_year_rf, "analysis_results_region_year_rf.csv", fileEncoding = "UTF-8")

# 9.5 8（年度ダミー）と9（地域×年度FE）の処置係数を並べて比較する
cmp <- function(res) {
  nm <- rownames(res)[res[, "指標"] == "logit係数"]
  k <- grep(treat_pat_ry, nm)
  setNames(data.frame(res[res[, "指標"] == "logit係数", "両方"][k],
                      res[res[, "指標"] == "Holm補正p値", "両方"][k],
                      row.names = nm[k]), c("係数", "Holm"))
}

compare_89 <- cbind(cmp(result_region_year), cmp(result_region_year_rf))
names(compare_89) <- c("8_係数", "8_Holm", "9_係数", "9_Holm")

compare_89

write.csv(compare_89, "analysis_compare_year_fe.csv", fileEncoding = "UTF-8")

#=======================================

# 10. 政令指定都市ダミーとセクション2のモデルの合成
# 全国共通の効果に政令市の上乗せ分（交互作用）を加える。政令市ダミー自体は
# 時間不変なので自治体固定効果に吸収される

treat_pat_sd <- "_政令市$"

# 10.1 経過年度を処置変数とする
treat_c_sd <- paste("factor(議員経過年度) +", paste(sprintf("議員%d年_政令市", 1:3), collapse = " + "))
treat_p_sd <- paste("factor(首長経過年度) +", paste(sprintf("首長%d年_政令市", 1:3), collapse = " + "))

seirei_share <- fe3("logit教育費シェア", ctrl_share, treat_c_sd, treat_p_sd)

lapply(seirei_share, summary)

result_seirei <- rbind(
  cbind(指標 = "logit係数", tab_of(seirei_share)),
  cbind(指標 = sprintf("教育費シェア%%pt (評価点 %.4f)", p_bar), tab_of(seirei_share, "share", p_bar)),
  cbind(指標 = "補正前p値", tab_of(seirei_share, "p", pat = treat_pat_sd)),
  cbind(指標 = "Holm補正p値", tab_of(seirei_share, "holm", pat = treat_pat_sd)),
  cbind(指標 = "モデル情報", info_of(seirei_share)))

result_seirei

write.csv(result_seirei, "analysis_results_seirei.csv", fileEncoding = "UTF-8")

# 10.2 経過年度の代わりに選挙年ダミーを処置変数とする
treat_c_sy <- "議員選挙年 + 議員選挙年_政令市"
treat_p_sy <- "首長選挙年 + 首長選挙年_政令市"

seirei_year <- fe3("logit教育費シェア", ctrl_share, treat_c_sy, treat_p_sy)

lapply(seirei_year, summary)

result_seirei_year <- rbind(
  cbind(指標 = "logit係数", tab_of(seirei_year)),
  cbind(指標 = sprintf("教育費シェア%%pt (評価点 %.4f)", p_bar), tab_of(seirei_year, "share", p_bar)),
  cbind(指標 = "補正前p値", tab_of(seirei_year, "p", pat = treat_pat_sd)),
  cbind(指標 = "Holm補正p値", tab_of(seirei_year, "holm", pat = treat_pat_sd)),
  cbind(指標 = "モデル情報", info_of(seirei_year)))

result_seirei_year

write.csv(result_seirei_year, "analysis_results_seirei_year.csv", fileEncoding = "UTF-8")

#=======================================

# 11. wild cluster bootstrap（少数クラスターで識別される係数の検定）
# 政令市は20市しかないため、解析的なクラスター頑健標準誤差は信頼できない
# within変換後のデータに対し、帰無仮説を課した残差をRademacher重みで撹乱する

wcb <- function(m, var, B = 999, seed = 1) {
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

# 11.1 政令市の交互作用について、解析的p値と並べて比較する
wcb_vars <- list(c("seirei_share", "議員1年_政令市"), c("seirei_share", "議員2年_政令市"),
                 c("seirei_share", "議員3年_政令市"), c("seirei_share", "首長1年_政令市"),
                 c("seirei_share", "首長2年_政令市"), c("seirei_share", "首長3年_政令市"),
                 c("seirei_year", "議員選挙年_政令市"), c("seirei_year", "首長選挙年_政令市"))

result_wcb <- t(sapply(wcb_vars, function(z) {
  m <- get(z[1])$両方
  ct <- coeftest(m, vcov = cl(m))[z[2], ]
  c(モデル = if (z[1] == "seirei_share") "10.1 経過年度" else "10.2 選挙年",
    係数 = sprintf("%.4f", ct[1]),
    クラスター頑健p値 = sprintf("%.4f", ct[4]),
    ブートストラップp値 = sprintf("%.4f", wcb(m, z[2])[2]))
}))

rownames(result_wcb) <- sapply(wcb_vars, `[`, 2)

result_wcb

write.csv(result_wcb, "analysis_results_wcb.csv", fileEncoding = "UTF-8")

#=======================================

# 12. 10・11の特別区（東京23区）バージョン

treat_pat_tk <- "_特別区$"

# 12.1 経過年度を処置変数とする
treat_c_tk <- paste("factor(議員経過年度) +", paste(sprintf("議員%d年_特別区", 1:3), collapse = " + "))
treat_p_tk <- paste("factor(首長経過年度) +", paste(sprintf("首長%d年_特別区", 1:3), collapse = " + "))

tokubetsu_share <- fe3("logit教育費シェア", ctrl_share, treat_c_tk, treat_p_tk)

lapply(tokubetsu_share, summary)

result_tokubetsu <- rbind(
  cbind(指標 = "logit係数", tab_of(tokubetsu_share)),
  cbind(指標 = sprintf("教育費シェア%%pt (評価点 %.4f)", p_bar), tab_of(tokubetsu_share, "share", p_bar)),
  cbind(指標 = "補正前p値", tab_of(tokubetsu_share, "p", pat = treat_pat_tk)),
  cbind(指標 = "Holm補正p値", tab_of(tokubetsu_share, "holm", pat = treat_pat_tk)),
  cbind(指標 = "モデル情報", info_of(tokubetsu_share)))

result_tokubetsu

write.csv(result_tokubetsu, "analysis_results_tokubetsu.csv", fileEncoding = "UTF-8")

# 12.2 経過年度の代わりに選挙年ダミーを処置変数とする
tokubetsu_year <- fe3("logit教育費シェア", ctrl_share,
                      "議員選挙年 + 議員選挙年_特別区", "首長選挙年 + 首長選挙年_特別区")

lapply(tokubetsu_year, summary)

result_tokubetsu_year <- rbind(
  cbind(指標 = "logit係数", tab_of(tokubetsu_year)),
  cbind(指標 = sprintf("教育費シェア%%pt (評価点 %.4f)", p_bar), tab_of(tokubetsu_year, "share", p_bar)),
  cbind(指標 = "補正前p値", tab_of(tokubetsu_year, "p", pat = treat_pat_tk)),
  cbind(指標 = "Holm補正p値", tab_of(tokubetsu_year, "holm", pat = treat_pat_tk)),
  cbind(指標 = "モデル情報", info_of(tokubetsu_year)))

result_tokubetsu_year

write.csv(result_tokubetsu_year, "analysis_results_tokubetsu_year.csv", fileEncoding = "UTF-8")

# 12.3 特別区も23区しかないのでwild cluster bootstrapで検定し直す
wcb_vars_tk <- list(c("tokubetsu_share", "議員1年_特別区"), c("tokubetsu_share", "議員2年_特別区"),
                    c("tokubetsu_share", "議員3年_特別区"), c("tokubetsu_share", "首長1年_特別区"),
                    c("tokubetsu_share", "首長2年_特別区"), c("tokubetsu_share", "首長3年_特別区"),
                    c("tokubetsu_year", "議員選挙年_特別区"), c("tokubetsu_year", "首長選挙年_特別区"))

result_wcb_tk <- t(sapply(wcb_vars_tk, function(z) {
  m <- get(z[1])$両方
  ct <- coeftest(m, vcov = cl(m))[z[2], ]
  c(モデル = if (z[1] == "tokubetsu_share") "12.1 経過年度" else "12.2 選挙年",
    係数 = sprintf("%.4f", ct[1]),
    クラスター頑健p値 = sprintf("%.4f", ct[4]),
    ブートストラップp値 = sprintf("%.4f", wcb(m, z[2])[2]))
}))

rownames(result_wcb_tk) <- sapply(wcb_vars_tk, `[`, 2)

result_wcb_tk

write.csv(result_wcb_tk, "analysis_results_wcb_tokubetsu.csv", fileEncoding = "UTF-8")
