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
#

