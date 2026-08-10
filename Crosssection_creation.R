rm(list = ls())
cat("\014")

# クロスセクションデータを作成する
# 統計データ(data/local_gov_statistics.csv)を利用して、自治体の基本情報を取得
# 統計データのうち用いるもののみを用いる＋そこから新たな変数を作成
# 統計データの外れ値を確認し、必要に応じて落とすべき自治体を見つける
# 統計データの分布を確認し、必要に応じて変換を行う
# 選挙データ(data/voting.csv)から、本選挙と補選の区切り付けを行う
# 選挙後何年目かの値を各自治体各年度に付与する。
# 最終的に、すべての自治体におけるすべての年度のデータを作成する。

# 1. 統計データから必要な情報を取得する

# 1.1 データの読み込み

locstats_all <- read.csv("data/local_gov_statistics.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
dim(locstats_all[locstats_all$年度 == 2018,])

# 1.2 利用する変数のみを抽出
# 地域コード、年度、教育費（市町村財政）、小学校児童数、中学校生徒数、算出決算総額、経常収支比率、実質公債費比率、総人口、総人口.女、X65歳以上人口
use_cols <- c("地域コード", "年度",
              "教育費.市町村財政.", "歳出決算総額.市町村財政.",
              "経常収支比率.市町村財政.", "実質公債費比率.市町村財政.",
              "小学校児童数", "中学校生徒数",
              "総人口", "総人口.女.", "X65歳以上人口")

locstats <- locstats_all[, use_cols]

# 1.3 地域女性割合と地域65歳以上割合の列を作成（人口データの欠損がないところのみ、それ以外は欠損値NA）
locstats$地域女性割合 <- locstats$総人口.女. / locstats$総人口
locstats$地域65歳以上割合 <- locstats$X65歳以上人口 / locstats$総人口

# 1.4 人口データについて、2015年と2020年の国勢調査から線形補間する
# 2016-2019は内挿、2021は2015->2020と同じ年あたりのペースで外挿
pop_vars <- c("総人口", "総人口.女.", "X65歳以上人口")

locstats15 <- locstats[locstats$年度 == 2015, ]
locstats20 <- locstats[locstats$年度 == 2020, ]

v15 <- locstats15[match(locstats$地域コード, locstats15$地域コード), pop_vars]
v20 <- locstats20[match(locstats$地域コード, locstats20$地域コード), pop_vars]

# 2015を0、2020を1とした位置。2021は1.2となり2020+(2020-2015)/5になる
w <- (locstats$年度 - 2015) / 5
i <- which(locstats$年度 %in% c(2016:2019, 2021))

locstats[i, pop_vars] <- v15[i, ] + (v20[i, ] - v15[i, ]) * w[i]

# 1.3の割合は補間後の人口に連動させる
locstats$地域女性割合 <- locstats$総人口.女. / locstats$総人口
locstats$地域65歳以上割合 <- locstats$X65歳以上人口 / locstats$総人口

# 1.5 2016-2021データに限定する
locstats_als <- locstats[locstats["年度"] >= 2016 & locstats["年度"] <= 2021,]

#================================================================================================

# 2.統計データの欠損値対応

# 2.1 統計データ(locstats_als)の確認

print(summary(locstats_als))
print(colSums(is.na(locstats_als)))

# 2.2 地域女性割合や65歳以上割合が欠損となるデータの確認 -> 東日本大震災の被災地が欠損値

print(locstats_als[is.na(locstats_als$地域女性割合) == TRUE,])

# 2.3 生徒数が欠損となるデータの確認 -> 2018年から市制を施行した福岡県那珂川市が欠損
print(locstats_als[is.na(locstats_als$小学校児童数) == TRUE,])

# 2.4 教育費.市町村財政.が欠損となるデータの確認 -> 2018年から市制を施行した福岡県那珂川市が欠損
print(locstats_als[is.na(locstats_als$教育費.市町村財政.) == TRUE,])

# 2.5 以下の東日本大震災の被災地をデータから除外（人口データが外生的な影響を受けているから）

#警戒区域
#富岡町、大熊町、双葉町のそれぞれ全域、田村市、南相馬市、楢葉町、川内村、浪江町、葛尾村のそれぞれ一部

#計画的避難区域
#浪江町、葛尾村の警戒区域を除いた区域、飯舘村全域、南相馬市の警戒区域を除いた一部、川俣町の一部

#緊急時避難準備区域
#広野町・楢葉町・川内村、および田村市と南相馬市の一部のうち、福島第一原子力発電所から半径20キロメートル圏外の地域

# 3区域に一部でも含まれる自治体（一部区域の指定でも自治体単位で除外）
hisaichi <- c("R07211", "R07212", "R07308", "R07541", "R07542", "R07543",
              "R07544", "R07545", "R07546", "R07547", "R07548", "R07564")
# 田村市・南相馬市・川俣町・広野町・楢葉町・富岡町・川内村・大熊町・双葉町・浪江町・葛尾村・飯舘村

locstats_als <- locstats_als[locstats_als$地域コード %in% hisaichi == FALSE, ]

colSums(is.na(locstats_als))

# 2.6 那珂川町(R40305)と那珂川市(R40231)は2018年の市制施行を挟む同一自治体なので、
# 町のデータを市のコードに寄せて年度をつなげ、町のコードは削除する
fill_vars <- setdiff(names(locstats_als), c("地域コード", "年度"))

i <- which(locstats_als$地域コード == "R40231")
from <- locstats_als[locstats_als$地域コード == "R40305", ]
src <- as.matrix(from[match(locstats_als$年度[i], from$年度), fill_vars])
tgt <- as.matrix(locstats_als[i, fill_vars])
tgt[is.na(tgt)] <- src[is.na(tgt)]
locstats_als[i, fill_vars] <- tgt

locstats_als <- locstats_als[locstats_als$地域コード != "R40305", ]

#=====================================================================================

# 3.統計データへ合成変数を作成

# 3.1 児童生徒数（小学校+中学校）
locstats_als$児童生徒数 <- locstats_als$小学校児童数 + locstats_als$中学校生徒数

# 3.2 教育費シェア
locstats_als$教育費シェア <- locstats_als$教育費.市町村財政. / locstats_als$歳出決算総額.市町村財政.

# 3.3 児童生徒割合
locstats_als$児童生徒割合 <- locstats_als$児童生徒数 / locstats_als$総人口

#=====================================================================================

# 4.統計データの分布を確認する

# ヒストグラムを年度ごとに分けて並べ、最後に全年度の箱ひげ図を描く
plot2 <- function(v) {
  par(mfrow = c(2, 4))
  for (y in sort(unique(locstats_als$年度)))
    hist(locstats_als[[v]][locstats_als$年度 == y], freq = FALSE,
         xlim = range(locstats_als[[v]]), main = paste(v, y), xlab = v)
  boxplot(locstats_als[[v]] ~ locstats_als$年度, main = v, xlab = "年度", ylab = v)
}

# 右に大きく歪む金額・人数系はlogを取る
log_vars <- c("教育費.市町村財政.", "歳出決算総額.市町村財政.",
              "総人口", "総人口.女.", "X65歳以上人口")

for (v in log_vars) locstats_als[[paste0("ln", v)]] <- log(locstats_als[[v]])

# 児童生徒数は0の自治体があるためlog1pを取る
locstats_als$ln児童生徒数 <- log1p(locstats_als$児童生徒数)

# 教育費シェアは0-1に収まる割合なのでlogitを取る（0も1も存在しないのでそのまま変換できる）
locstats_als$logit教育費シェア <- log(locstats_als$教育費シェア / (1 - locstats_als$教育費シェア))

# 4.1 教育費、4.2 歳出決算総額、4.5 総人口とその他人口関連、児童生徒数（それぞれlog前後）
for (v in c(log_vars, "児童生徒数")) { plot2(v); plot2(paste0("ln", v)) }

# 4.3 経常収支比率、4.4 実質公債費比率（負値あり）、割合系はlogなし
for (v in c("経常収支比率.市町村財政.", "実質公債費比率.市町村財政.", "児童生徒割合",
            "地域女性割合", "地域65歳以上割合", "教育費シェア", "logit教育費シェア")) plot2(v)

# 経常収支比率の外れ値を確認（100超は経常経費が経常一般財源を上回る硬直状態、60未満は原発・観光等の税収が突出）
kj <- locstats_als$経常収支比率.市町村財政.

quantile(kj, c(0, .001, .01, .5, .99, .999, 1))

locstats_als[which(kj > 110), ]
locstats_als[which(kj < 60), ]

length(unique(locstats_als$地域コード[which(kj > 100)]))
length(unique(locstats_als$地域コード[which(kj < 60)]))

# 経常収支比率の箱ひげ図を単独で大きく描き、夕張市(R01209)を赤で重ねる
# （100%が財政硬直の目安。この時点ではまだ4.5の除外前なので夕張市も含まれている）
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))

boxplot(kj ~ locstats_als$年度, xlab = "年度", ylab = "経常収支比率 (%)",
        cex.lab = 1.4, cex.axis = 1.2)
abline(h = 100, lty = 2, col = "gray40")

yubari <- locstats_als[locstats_als$地域コード == "R01209", ]
tomari <- locstats_als[locstats_als$地域コード == "R01403", ]
xpos <- match(yubari$年度, sort(unique(locstats_als$年度)))
xpos2 <- match(tomari$年度, sort(unique(locstats_als$年度)))

points(xpos, yubari$経常収支比率.市町村財政., col = "red", pch = 19)
points(xpos, tomari$経常収支比率.市町村財政., col = "blue", pch = 19)


# 4.5 実質公債費比率の外れ値を確認 -> 夕張市が異常な値
locstats_als[locstats_als$実質公債費比率.市町村財政. > 40,]

# 夕張市(R01209)を除外
locstats_als <- locstats_als[locstats_als$地域コード != "R01209", ]

summary(locstats_als[, c("実質公債費比率.市町村財政.", "経常収支比率.市町村財政.")])

# 4.6 児童生徒数が0の自治体を確認(18自治体を削除)
locstats_als[locstats_als$児童生徒数 == 0,]

# 1年でも0の年がある自治体は全年度まとめて削除
zero_cd <- unique(locstats_als$地域コード[locstats_als$児童生徒数 == 0])

locstats_als <- locstats_als[locstats_als$地域コード %in% zero_cd == FALSE, ]

# 4.7 教育費シェアが高い自治体を確認
locstats_als[locstats_als$教育費シェア > 0.35,]

# 4.8 再びヒストグラムと箱ひげ図、バイオリンプロットを出力
library(lattice)

all_vars <- c(log_vars, paste0("ln", log_vars), "児童生徒数", "ln児童生徒数",
              "経常収支比率.市町村財政.", "実質公債費比率.市町村財政.",
              "地域女性割合", "地域65歳以上割合", "教育費シェア", "logit教育費シェア")

for (v in all_vars) {
  plot2(v)
  print(bwplot(factor(locstats_als$年度) ~ locstats_als[[v]], panel = panel.violin,
               main = v, xlab = v, ylab = "年度"))
}

# 4.9 教育費や総人口、経常収支比率、児童生徒数の基本統計量（特に、歪度）を確認し歪みを数値化
skew <- function(x) mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5

stat_vars <- c("教育費.市町村財政.", "ln教育費.市町村財政.",
               "総人口", "ln総人口",
               "児童生徒数", "ln児童生徒数",
               "経常収支比率.市町村財政.",
               "教育費シェア", "logit教育費シェア")

round(t(sapply(locstats_als[, stat_vars], function(x)
  c(平均 = mean(x), 中央値 = median(x), 標準偏差 = sd(x),
    最小 = min(x), 最大 = max(x), 歪度 = skew(x)))), 3)

# 4.10 教育費・総人口・児童生徒数・教育費シェアについて、上段に生データ、下段に変換後を並べる（2016年度）
d16 <- locstats_als[locstats_als$年度 == 2016, ]

raw_v <- c("教育費.市町村財政.", "総人口", "児童生徒数", "教育費シェア")
tr_v <- c("ln教育費.市町村財政.", "ln総人口", "ln児童生徒数", "logit教育費シェア")

par(mfrow = c(2, 4))
for (v in raw_v) hist(d16[[v]], breaks = 30, freq = FALSE, main = v, xlab = v)
for (v in tr_v) hist(d16[[v]], breaks = 30, freq = FALSE, main = v, xlab = v)

#================================================================================
# 5. 選挙データの区切り付け（議会議員）

# 5.1 データの読み込み
voting_data <- read.csv("data/voting.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
voting_data$投票日　<- as.Date(voting_data$投票日, format = "%Y-%m-%d")

# 5.2 データから年・月を切り出す
voting_data$年 <- format(voting_data$投票日, "%Y")
voting_data$月 <- format(voting_data$投票日, "%m")

# 5.3 選挙年度を新たな列に入れる（4月から次の年の3月までがその年の会計年度）
voting_data$選挙年度 <- as.integer(voting_data$年) - (as.integer(voting_data$月) < 4)

# 5.4 データを選挙タイプごとに分ける
voting_data_c <- voting_data[voting_data$議会議員 == 1,]

voting_data_c_0 <- voting_data_c[voting_data_c$補欠選挙 == 0 & voting_data_c$再選挙 == 0,]

# 5.5 議会議員選挙については、本選については前回の本選からどれだけ経ったのかを入れる（最初の本選はNA）
main_d <- split(as.numeric(voting_data_c_0$投票日), voting_data_c_0$地域コード)

voting_data_c_0$前回間隔 <- mapply(function(cd, d) {
  x <- main_d[[cd]][main_d[[cd]] < d]
  if (length(x)) d - max(x) else NA
}, voting_data_c_0$地域コード, as.numeric(voting_data_c_0$投票日))

summary(voting_data_c_0$前回間隔)

# 5.6 前回の本選からの日数が1277以上となっているデータとNAとなっている本選挙に1を入れるダミー変数（間隔平常ダミー）を作る
# 1277日(=3.5年)未満は任期途中の解散・合併等で周期が乱れたもの、NAは直前の本選がデータ期間外のもの
voting_data_c_0$間隔平常 <- as.integer(is.na(voting_data_c_0$前回間隔) | voting_data_c_0$前回間隔 >= 1277)

table(voting_data_c_0$間隔平常)
voting_data_c_0[which(voting_data_c_0$前回間隔 < 1277), ]

# 5.7 voting_data_cにも間隔平常ダミーを還元する。補欠・再選挙には間隔平常ダミーに0を入れる
voting_data_c$間隔平常 <- 0L
voting_data_c$間隔平常[voting_data_c$補欠選挙 == 0 & voting_data_c$再選挙 == 0] <- voting_data_c_0$間隔平常

table(voting_data_c$間隔平常, voting_data_c$補欠選挙 + voting_data_c$再選挙)

#=====================================================================================
# 6. 選挙データの区切り付け（首長）

# 6.1 首長データを分ける
voting_data_p <- voting_data[voting_data$首長 == 1,]

# 6.2 前回の選挙（本選挙や補欠選挙、再選挙にかかわらず）からどれだけ経ったのかを入れる
# 参照先が自分自身なので、自治体ごとに投票日を並べて差分を取る（最初の選挙はNA）
voting_data_p <- voting_data_p[order(voting_data_p$地域コード, voting_data_p$投票日), ]

voting_data_p$前回間隔 <- ave(as.numeric(voting_data_p$投票日), voting_data_p$地域コード,
                              FUN = function(x) c(NA, diff(x)))

summary(voting_data_p$前回間隔)

# 6.3 前回の選挙からの日数が1277以上となっているデータとNAとなっているものに1を入れるダミー変数（間隔平常ダミー）を作る
voting_data_p$間隔平常 <- as.integer(is.na(voting_data_p$前回間隔) | voting_data_p$前回間隔 >= 1277)

#=================================================================================
# 7. 議員選挙データをlocstats_alsに導入

# 7.1 議員選挙データのうち、間隔平常が1のデータのみ取ってくる
voting_data_c_n <- voting_data_c[voting_data_c$間隔平常 == 1,]

# 7.2 locstats_alsの自治体について、2012-2021の地域コード×年度の枠を作る
# （前年度の値で埋める際に2016年度より前の選挙も参照できるようにするため）
locstats_als_comb <- expand.grid(地域コード = unique(locstats_als$地域コード),
                                 年度 = 2012:2021, stringsAsFactors = FALSE)

locstats_als_comb <- locstats_als_comb[order(locstats_als_comb$地域コード, locstats_als_comb$年度), ]
rownames(locstats_als_comb) <- NULL

# 7.3 locstats_als_combの地域コード、年度とvoting_data_c_nの地域コード、選挙年度が同じ場合にvoting_data_c_nの変数のうち選挙の性質を示す変数を入れる。
# それ以外のものはNAを入れる。
el_vars <- c("候補者数", "年齢中央値", "年齢最小値", "年齢平均値", "X65歳以上割合",
             "新人割合", "女性割合", "自民公明割合", "共産社民割合", "無所属割合")

i <- match(paste(locstats_als_comb$地域コード, locstats_als_comb$年度),
           paste(voting_data_c_n$地域コード, voting_data_c_n$選挙年度))

locstats_als_comb[, el_vars] <- voting_data_c_n[i, el_vars]

# 7.4 選挙年ダミーを立ててから、選挙の性質を前年度送りで埋め、選挙後何年目かを入れる

# 埋める前に立てないと全行1になってしまう
locstats_als_comb$選挙年 <- as.integer(is.na(locstats_als_comb$候補者数) == FALSE)

# 直前の非NAの位置を cummax で拾って埋める（それ以前に選挙がなければNAのまま）
fill_prev <- function(x) x[pmax(cummax((is.na(x) == FALSE) * seq_along(x)), 1)]

for (v in el_vars)
  locstats_als_comb[[v]] <- ave(locstats_als_comb[[v]], locstats_als_comb$地域コード, FUN = fill_prev)

# 選挙のあった年度を0とする
locstats_als_comb$経過年度 <- ave(locstats_als_comb$選挙年, locstats_als_comb$地域コード,
                                  FUN = function(x) {
                                    i <- cummax(x * seq_along(x))
                                    ifelse(i == 0, NA, seq_along(x) - i)
                                  })

# 7.5 間隔平常ダミーが0の選挙がある際、その地域のその年に新たなダミー変数を作り、1をとる。それ以外は0。
locstats_als_comb$間隔異常 <- as.integer(
  paste(locstats_als_comb$地域コード, locstats_als_comb$年度) %in%
    paste(voting_data_c$地域コード, voting_data_c$選挙年度)[voting_data_c$間隔平常 == 0])

# 7.6 locstats_alsに議員データとして明記する形でlocstats_als_combのデータを結合する。

# 政令指定都市・特別区は選挙ではなく自治体の属性なので、地域コードから直接付ける
attr_vars <- c("都道府県", "地域", "政令指定都市", "特別区")
locstats_als[, attr_vars] <- voting_data[match(locstats_als$地域コード, voting_data$地域コード), attr_vars]

comb_vars <- c(el_vars, "選挙年", "経過年度", "間隔異常")

k <- match(paste(locstats_als$地域コード, locstats_als$年度),
           paste(locstats_als_comb$地域コード, locstats_als_comb$年度))

locstats_als[, paste0("議員", comb_vars)] <- locstats_als_comb[k, comb_vars]


#=================================================================================
# 8. 首長選挙データをlocstats_alsに導入（7と同じ手順を首長データに対して行う）

# 8.1 首長は間隔異常な選挙も選挙として数えるため、全選挙を使う
voting_data_p_n <- voting_data_p

# 8.2 2012-2021の地域コード×年度の枠を作る
locstats_als_comb_p <- expand.grid(地域コード = unique(locstats_als$地域コード),
                                   年度 = 2012:2021, stringsAsFactors = FALSE)

locstats_als_comb_p <- locstats_als_comb_p[order(locstats_als_comb_p$地域コード, locstats_als_comb_p$年度), ]
rownames(locstats_als_comb_p) <- NULL

# 8.3 地域コード・年度と地域コード・選挙年度が一致する場合に選挙の性質を示す変数を入れる
# 同じ年度に両方ある場合は、間隔平常=1を入れたあと間隔平常=0で上書きする
locstats_als_comb_p[, el_vars] <- NA_real_

for (nm in c(1, 0)) {
  src <- voting_data_p_n[voting_data_p_n$間隔平常 == nm, ]
  i <- match(paste(locstats_als_comb_p$地域コード, locstats_als_comb_p$年度),
             paste(src$地域コード, src$選挙年度))
  locstats_als_comb_p[which(is.na(i) == FALSE), el_vars] <- src[i[is.na(i) == FALSE], el_vars]
}

# 8.4 選挙年ダミーを立ててから、選挙の性質を前年度送りで埋め、選挙後何年目かを入れる
locstats_als_comb_p$選挙年 <- as.integer(is.na(locstats_als_comb_p$候補者数) == FALSE)

for (v in el_vars)
  locstats_als_comb_p[[v]] <- ave(locstats_als_comb_p[[v]], locstats_als_comb_p$地域コード, FUN = fill_prev)

locstats_als_comb_p$経過年度 <- ave(locstats_als_comb_p$選挙年, locstats_als_comb_p$地域コード,
                                    FUN = function(x) {
                                      i <- cummax(x * seq_along(x))
                                      ifelse(i == 0, NA, seq_along(x) - i)
                                    })

# 8.5 間隔平常ダミーが0の選挙がある地域・年に1をとるダミー変数を作る
locstats_als_comb_p$間隔異常 <- as.integer(
  paste(locstats_als_comb_p$地域コード, locstats_als_comb_p$年度) %in%
    paste(voting_data_p$地域コード, voting_data_p$選挙年度)[voting_data_p$間隔平常 == 0])

# 8.6 locstats_alsに首長データとして明記する形で結合する
k <- match(paste(locstats_als$地域コード, locstats_als$年度),
           paste(locstats_als_comb_p$地域コード, locstats_als_comb_p$年度))

locstats_als[, paste0("首長", comb_vars)] <- locstats_als_comb_p[k, comb_vars]

#=================================================================================

# 9. クロスセクションデータを確認する

# 9.0 合成変数（議会高齢化ギャップと議会女性ギャップ）を作成

locstats_als$議会高齢化ギャップ <- locstats_als$議員X65歳以上割合 - locstats_als$地域65歳以上割合
locstats_als$議会女性ギャップ <- locstats_als$議員女性割合 - locstats_als$地域女性割合

# 9.1 構造と欠損
str(locstats_als)
summary(locstats_als)
colSums(is.na(locstats_als))

# 9.2 自治体×年度が過不足なくそろっているか（全自治体が6年分あれば均衡パネル）
table(table(locstats_als$地域コード))
table(locstats_als$年度)

# 9.3 選挙関連の分布（議員と首長の選挙周期は独立しているはず）
table(議員 = locstats_als$議員経過年度, 首長 = locstats_als$首長経過年度)
table(議員選挙年 = locstats_als$議員選挙年, 首長選挙年 = locstats_als$首長選挙年)
table(議員間隔異常 = locstats_als$議員間隔異常, 首長間隔異常 = locstats_als$首長間隔異常)

# 9.4 経過年度が4以上（任期を超えている）自治体の確認
over4 <- locstats_als[locstats_als$議員経過年度 >= 4 | locstats_als$首長経過年度 >= 4,
                      c("地域コード", "年度", "議員経過年度", "首長経過年度")]

over4[, c("都道府県", "市区町村")] <- voting_data[match(over4$地域コード, voting_data$地域コード),
                                                  c("都道府県", "市区町村")]

over4

# 9.5 前回3月・今回4月で会計年度をまたいだために4となった行のみ3に丸める
# （実際の選挙間隔は1435-1484日でいずれも正常な4年周期）
# 上郡町・太宰府市・市川市は実間隔自体が長いのでそのまま残す
round3_c <- c("R28212_2016", "R41327_2021")
round3_p <- c("R01631_2016", "R02307_2020", "R09386_2020", "R24461_2017",
              "R36201_2019", "R40608_2016", "R41327_2021", "R43211_2017")

key <- paste(locstats_als$地域コード, locstats_als$年度, sep = "_")

locstats_als$議員経過年度[key %in% round3_c] <- 3
locstats_als$首長経過年度[key %in% round3_p] <- 3

table(議員 = locstats_als$議員経過年度)
table(首長 = locstats_als$首長経過年度)

# 9.6 実間隔自体が長く丸められない自治体（市川市・上郡町・太宰府市）は均衡パネルを保つため丸ごと落とす
drop_cd <- unique(locstats_als$地域コード[locstats_als$議員経過年度 > 3 | locstats_als$首長経過年度 > 3])

locstats_als <- locstats_als[locstats_als$地域コード %in% drop_cd == FALSE, ]
rownames(locstats_als) <- NULL


# 9.7 パネルデータを出力する
write.csv(locstats_als, "data/crosssection.csv", row.names = FALSE, fileEncoding = "UTF-8")

#=============================================================================

# 10. データのビジュアライズ

# 10.1 主要変数の基本統計量（総人口と児童生徒数はlogを取ったもの）
desc_vars <- c("ln教育費.市町村財政.", "logit教育費シェア",
               "議員X65歳以上割合", "議会高齢化ギャップ", "議員女性割合", "議会女性ギャップ",
               "議員新人割合", "首長X65歳以上割合", "首長女性割合", "首長新人割合",
               "ln総人口", "地域65歳以上割合", "地域女性割合", "経常収支比率.市町村財政.",
               "ln児童生徒数", "児童生徒割合")

desc_stats <- round(t(sapply(locstats_als[, desc_vars], function(x)
  c(平均 = mean(x), 標準偏差 = sd(x), 最小値 = min(x),
    第1四分位 = unname(quantile(x, .25)), 中央値 = median(x),
    第3四分位 = unname(quantile(x, .75)), 最大値 = max(x)))), 3)

desc_stats

write.csv(desc_stats, "descriptive_stats.csv", fileEncoding = "UTF-8")

# 10.2 年度ごとの選挙数（選挙のあった自治体の数）
elec_tab <- with(locstats_als, cbind(議員選挙 = tapply(議員選挙年, 年度, sum),
                                     首長選挙 = tapply(首長選挙年, 年度, sum)))

elec_tab <- cbind(elec_tab, 合計 = rowSums(elec_tab))

elec_tab

write.csv(elec_tab, "election_counts.csv", fileEncoding = "UTF-8")

# 10.3 10.2の表を横向きの棒グラフにする（年度を縦に並べた縦長の図）
par(mfrow = c(1, 1), mar = c(5, 6, 3, 2))

# horiz=TRUEは先頭列を下に描くので、2016が上に来るよう年度を逆順にする
rev_tab <- t(elec_tab[nrow(elec_tab):1, c("議員選挙", "首長選挙")])

bp <- barplot(rev_tab, beside = TRUE, horiz = TRUE, las = 1,
              xlim = c(0, 1050), xlab = "選挙のあった自治体数",
              col = c("red", "blue"), cex.lab = 1.3, cex.axis = 1.1, cex.names = 1.2)

text(rev_tab, bp, labels = rev_tab, pos = 4, cex = 0.9)

legend("bottomright", legend = c("議員選挙", "首長選挙"), fill = c("red", "blue"), bty = "n", cex = 1.1)

# 10.4 主要変数の相関ヒートマップ（金額・人数はlog、教育費シェアはlogit変換後）
cor_vars <- c("ln教育費.市町村財政.", "logit教育費シェア",
              "議員X65歳以上割合", "議会高齢化ギャップ", "議員女性割合", "議会女性ギャップ",
              "議員新人割合", "首長X65歳以上割合", "首長女性割合", "首長新人割合",
              "ln総人口", "地域65歳以上割合", "地域女性割合", "経常収支比率.市町村財政.",
              "ln児童生徒数", "児童生徒割合")

cor_labs <- c("ln(教育費)", "logit(教育費シェア)",
              "議員65歳以上割合", "議会高齢化ギャップ", "議員女性割合", "議会女性ギャップ",
              "議員新人割合", "首長65歳以上ダミー", "首長女性ダミー", "首長新人ダミー",
              "ln(総人口)", "地域65歳以上割合", "地域女性割合", "経常収支比率",
              "ln(児童生徒数)", "児童生徒割合")

cm <- cor(locstats_als[, cor_vars])
dimnames(cm) <- list(cor_labs, cor_labs)

# levelplotはy軸を下から描くので、列を逆順にして1変数目が左上に来るようにする
print(levelplot(cm[, ncol(cm):1], xlab = "", ylab = "",
                at = seq(-1, 1, length.out = 41),
                col.regions = colorRampPalette(c("blue", "white", "red"))(40),
                scales = list(x = list(rot = 90, cex = 0.8), y = list(cex = 0.8)),
                panel = function(x, y, z, ...) {
                  panel.levelplot(x, y, z, ...)
                  panel.text(x, y, round(z, 2), cex = 0.55)
                }))






