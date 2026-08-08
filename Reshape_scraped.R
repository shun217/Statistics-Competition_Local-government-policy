library(dplyr)

#===========================
# 1. 元データの基本形成

# 1-1. データの読み込み
base <- read.csv("data/seijiyama_result_all.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 1-2. いらない列の削除（告示日、detail_url、定数、執行理由、有権者数、投票率、前回投票率、得票数、主な肩書）
drop_cols <- c("告示日", "detail_url", "定数", "執行理由",
               "有権者数", "投票率", "前回投票率", "得票数", "主な肩書き")

base <- base[, setdiff(names(base), drop_cols), drop = FALSE]

# 1.3. 新たな列（データ上候補者数）を作成し、日付と選挙名が同じデータの数を入れる。
# 同名の町村（山形県小国町と熊本県小国町など）が同日に選挙をしているため都道府県も一致条件に含める
base <- base %>% add_count(投票日, 選挙名, 都道府県, name = "データ上候補者数")


# 1-4. electedした人のみのデータに限定する
base <- base[base$elected == "TRUE", ]

# 1-5. 選挙名から都道府県と市区町村を抽出（選挙名を後ろから見ていき、市区町村都道府県のワードが出たらそれより前の部分を取る）
name <- sub("（.*", "", base$選挙名)

base$市区町村 <- ifelse(grepl("[市区町村]", name),
                                sub("^(.*[市区町村]).*$", "\\1", name, perl = TRUE), NA)

base$都道府県_抽出 <- ifelse(is.na(base$市区町村) & grepl("[都道府県]", name),
                                     sub("^(.*[都道府県]).*$", "\\1", name, perl = TRUE), NA)

# 1-6.都道府県に関連する（都道府県抽出の列がNAでない）行データの削除
base_city <- base[is.na(base$都道府県_抽出), ]

# 市区町村名より後ろの部分（「議会議員補欠選挙」「長選挙」など）
suffix <- substring(sub("（.*", "", base_city$選挙名), nchar(base_city$市区町村) + 1)

# 1-7. 選挙名に議会議員と入っているもののダミー変数を作る
base_city$議会議員 <- as.integer(grepl("議会議員", suffix))

# 1-8. 長と入っているもののダミー変数を作る
base_city$首長 <- as.integer(grepl("長", suffix))

# 1-9. 補欠選挙のダミー変数を作る
base_city$補欠選挙 <- as.integer(grepl("補欠", suffix))

# 1-10. 再選挙のダミー変数を作る
base_city$再選挙 <- as.integer(grepl("再選挙", suffix))

# 1-11. 新たなdfを作成し、そこに、投票日・選挙名・市区町村・都道府県・議員ダミー・首長ダミー・補欠ダミー・候補者数・年齢・性別・党派・新旧をこの順番で入れる
before_agg <- base_city[, c("投票日", "選挙名", "市区町村", "都道府県",
                    "議会議員", "首長", "補欠選挙","再選挙","候補者数",
                    "年齢", "性別", "党派", "新旧", "データ上候補者数")]

#==============================================================
# 2.データを選挙ごとに集計

# 2-1. 投票日と選挙名が完全一致するものごとにまとめて新たなdf(af_agg)を作成する。
# 投票日、選挙名、市区町村、都道府県、議会議員ダミー、首長ダミー、補欠ダミー、
# 候補者数、年齢中央値、年齢最小値、男性数、女性数、新人割合、65歳以上割合、
# 自民公明割合（自民or公明というワードが党派にある人の割合）、共産社民割合、無所属割合を入れる
# 欠損が一つでもあるものはNAとして計算を停止する
# 空文字は欠損として扱う
before_agg <- before_agg %>% mutate(across(where(is.character), ~ na_if(.x, "")))

# grepl は NA を FALSE にしてしまうので、欠損は NA のまま返す
has <- function(x, pattern) ifelse(is.na(x), NA, grepl(pattern, x))

af_agg <- before_agg %>%
  group_by(投票日, 選挙名, 市区町村, 都道府県) %>%
  summarise(
    議会議員 = first(議会議員),
    首長 = first(首長),
    補欠選挙 = first(補欠選挙),
    再選挙 = first(再選挙),
    候補者数 = first(候補者数),
    年齢中央値 = median(年齢),
    年齢最小値 = min(年齢),
    年齢平均値 = mean(年齢),
    男性数 = sum(性別 == "男"),
    女性数 = sum(性別 == "女"),
    新人割合 = mean(新旧 == "新"),
    `65歳以上割合` = mean(年齢 >= 65),
    自民公明割合 = mean(has(党派, "自民|公明")),
    共産社民割合 = mean(has(党派, "共産|社民")),
    無所属割合 = mean(党派 == "無所属"),
    データ上候補者数 = first(データ上候補者数),
    .groups = "drop"
  )

# 2-2. NAとなっている列数を出力
print(colSums(is.na(af_agg)))

# 2-3. csvとして出力
write.csv(af_agg, "data/af_agg.csv", row.names = FALSE, fileEncoding = "UTF-8")
