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

# 1.2 利用する変数のみを抽出
# 地域コード、年度、教育費（市町村財政）、小学校児童数、中学校生徒数、算出決算総額、経常収支比率、実質公債費比率、総人口、総人口.女、X65歳以上人口
use_cols <- c("地域コード", "年度",
              "教育費.市町村財政.", "歳入決算総額.市町村財政.",
              "経常収支比率.市町村財政.", "実質公債費比率.市町村財政.",
              "小学校児童数", "中学校生徒数",
              "総人口", "総人口.女.", "X65歳以上人口")

locstats <- locstats_all[, use_cols]

# 1.3 地域女性割合と地域65歳以上割合の列を作成（人口データの欠損がないところのみ、それ以外は欠損値NA）
locstats$地域女性割合 <- locstats$総人口.女. / locstats$総人口
locstats$地域65歳以上割合 <- locstats$X65歳以上人口 / locstats$総人口

# 1.4 人口データについて、間を補完する。(2016-2019は2015データで、2021以降は2020で)
pop_vars <- c("総人口", "総人口.女.", "X65歳以上人口", "地域女性割合", "地域65歳以上割合")

locstats15 <- locstats[locstats$年度 == 2015, ]
locstats20 <- locstats[locstats$年度 == 2020, ]

i15 <- which(locstats$年度 %in% 2016:2019)
i20 <- which(locstats$年度 >= 2021)

locstats[i15, pop_vars] <- locstats15[match(locstats$地域コード[i15], locstats15$地域コード), pop_vars]
locstats[i20, pop_vars] <- locstats20[match(locstats$地域コード[i20], locstats20$地域コード), pop_vars]

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
locstats_als$教育費シェア <- locstats_als$教育費.市町村財政. / locstats_als$歳入決算総額.市町村財政.

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
log_vars <- c("教育費.市町村財政.", "歳入決算総額.市町村財政.",
              "総人口", "総人口.女.", "X65歳以上人口")

for (v in log_vars) locstats_als[[paste0("ln", v)]] <- log(locstats_als[[v]])

# 児童生徒数は0の自治体があるためlog1pを取る
locstats_als$ln児童生徒数 <- log1p(locstats_als$児童生徒数)

# 4.1 教育費、4.2 歳入決算総額、4.5 総人口とその他人口関連、児童生徒数（それぞれlog前後）
for (v in c(log_vars, "児童生徒数")) { plot2(v); plot2(paste0("ln", v)) }

# 4.3 経常収支比率、4.4 実質公債費比率（負値あり）、割合系はlogなし
for (v in c("経常収支比率.市町村財政.", "実質公債費比率.市町村財政.",
            "地域女性割合", "地域65歳以上割合", "教育費シェア")) plot2(v)

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
              "地域女性割合", "地域65歳以上割合", "教育費シェア")

for (v in all_vars) {
  plot2(v)
  print(bwplot(factor(locstats_als$年度) ~ locstats_als[[v]], panel = panel.violin,
               main = v, xlab = v, ylab = "年度"))
}

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

# 選挙のあった年度を1年目とする
locstats_als_comb$経過年度 <- ave(locstats_als_comb$選挙年, locstats_als_comb$地域コード,
                                  FUN = function(x) {
                                    i <- cummax(x * seq_along(x))
                                    ifelse(i == 0, NA, seq_along(x) - i + 1)
                                  })







