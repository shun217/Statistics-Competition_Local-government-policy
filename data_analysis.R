# ==========================================
# 1. ライブラリの読み込み
# ==========================================
library(tidyverse)   # データ操作・可視化
library(readxl)      # Excel読み込み
library(psych)       # 偏相関係数
library(corrplot)    # 相関行列の可視化
library(stats)       # PCA (prcomp)
library(plm)         # パネルデータ分析 (PanelOLSの代替)
library(ggrepel)     # グラフ内のラベル重なり防止
library(sandwich)    # 頑健な標準誤差
library(lmtest)      # 係数テスト

# ==========================================
# 2. データの読み取りと加工
# ==========================================
df <- read_csv("data/election_data.csv")
df_statistics <- read_csv("data/local_gov_statistics.csv")
df_region_code <- read_csv("data/region_code_correspondence.csv")

# 65歳以上人口割合の算出
abv65_df <- df_statistics %>%
  select(地域コード, 年度, 総人口, `65歳以上人口`) %>%
  mutate(`65歳以上人口割合` = `65歳以上人口` / 総人口)

abv65_meandf <- abv65_df %>%
  group_by(地域コード) %>%
  summarise(`65歳以上人口割合` = mean(`65歳以上人口割合`, na.rm = TRUE)) %>%
  left_join(df_region_code, by = "地域コード")

# データの結合
merged_df <- df %>%
  left_join(select(abv65_meandf, -都道府県), by = "市区町村") %>%
  mutate(
    日付 = as.Date(日付, format = "%Y/%m/%d"),
    年 = as.numeric(format(日付, "%Y"))
  )

# 議員と首長の分離
counciler_df <- merged_df %>% filter(種別 == "議員")
chief_df <- merged_df %>% filter(種別 == "首長")

# ==========================================
# 3. 欠損値補完 (男女比率)
# ==========================================
counciler_df <- counciler_df %>%
  group_by(市区町村) %>%
  arrange(日付) %>%
  mutate(
    男性 = ifelse(男性 == 0 & 女性 == 0, NA, 男性),
    女性 = ifelse(is.na(男性), NA, 女性)
  ) %>%
  fill(男性, 女性, .direction = "downup") %>%
  mutate(男女比率 = 女性 / (女性 + 男性)) %>%
  ungroup()

# ==========================================
# 4. 主成分分析 (PCA)
# ==========================================

# --- 議員 PCA ---
pca_input_c <- counciler_df %>%
  mutate(`65pp` = `65歳以上割合` - `65歳以上人口割合`) %>%
  select(`65pp`, 新人割合, 男女比率, `自民+公明割合`, `共産+社民割合`, 無所属割合) %>%
  drop_na()

pca_res_c <- prcomp(pca_input_c, scale. = TRUE)

pca_scores_c <- as.data.frame(pca_res_c$x[, 1:2]) %>%
  rename(CPC1 = PC1, CPC2 = PC2) %>%
  bind_cols(counciler_df[rownames(pca_input_c), c("市区町村", "年")]) %>%
  mutate(期 = 1)

# --- 首長 PCA ---
pca_input_ch <- chief_df %>%
  select(年齢最小値, 新人割合, 男性, `自民+公明割合`, `共産+社民割合`, 無所属割合) %>%
  drop_na()

pca_res_ch <- prcomp(pca_input_ch, scale. = TRUE)

pca_scores_ch <- as.data.frame(pca_res_ch$x[, 1:2]) %>%
  rename(CHPC1 = PC1, CHPC2 = PC2) %>%
  bind_cols(chief_df[rownames(pca_input_ch), c("市区町村", "年")]) %>%
  mutate(期 = 1)

# ==========================================
# 5. 回帰用データの作成 (df_final 相当)
# ==========================================

# 統計データのクリーニング
df_statistics_clean <- df_statistics %>%
  filter(!is.na(`教育費(市町村財政)`))

df_final <- df_statistics_clean %>%
  select(地域コード, 年度, `教育費(市町村財政)`, `歳入決算総額(市町村財政)`) %>%
  left_join(df_region_code %>% select(地域コード, 市区町村), by = "地域コード")

# PCA結果のマスタ作成とマージ
unique_cities <- unique(pca_scores_c$市区町村)
years <- 2012:2021
PCA_df_final <- expand.grid(市区町村 = unique_cities, 年度 = years)

All_PCA_df <- PCA_df_final %>%
  left_join(pca_scores_c %>% rename(年度 = 年, `期（議員）` = 期), by = c("市区町村", "年度")) %>%
  left_join(pca_scores_ch %>% rename(年度 = 年, `期（首長）` = 期), by = c("市区町村", "年度")) %>%
  group_by(市区町村) %>%
  arrange(年度) %>%
  # Pythonの累積cumsumロジックの再現
  mutate(
    `期（議員）` = ifelse(is.na(`期（議員）`), 0, `期（議員）`),
    `期（議員）` = cumsum(`期（議員）`),
    `期（首長）` = ifelse(is.na(`期（首長）`), 0, `期（首長）`),
    `期（首長）` = cumsum(`期（首長）`)
  ) %>%
  fill(CPC1, CPC2, CHPC1, CHPC2, .direction = "down") %>%
  ungroup() %>%
  filter(年度 >= 2015)

# 最終的なマージ
All_PCA_df <- All_PCA_df %>%
  left_join(df_final, by = c("市区町村", "年度")) %>%
  left_join(df_region_code %>% select(地域コード, 都道府県), by = "地域コード") %>%
  drop_na() %>%
  select(市区町村, 地域コード, 年度, 都道府県, CPC1, CPC2, `期（議員）`, CHPC1, CHPC2, `期（首長）`, `歳入決算総額(市町村財政)`, `教育費(市町村財政)`)

# ==========================================
# 6. 回帰分析 (OLS & パネル)
# ==========================================

df_OLS <- All_PCA_df %>%
  arrange(地域コード, 年度) %>%
  mutate(
    ln_教育費 = log(`教育費(市町村財政)`),
    ln_算出決算総額 = log(`歳入決算総額(市町村財政)`)
  ) %>%
  # 4期を超えるデータを落とす
  filter(`期（議員）` <= 4 & `期（首長）` <= 4)

# 期を因子型（Factor）に変換（1期目がベースライン）
df_OLS$期_議員_F <- factor(df_OLS$`期（議員）`)

# パネルデータセットの設定
p_df <- pdata.frame(df_OLS, index = c("市区町村", "年度"))

# モデル式: 交差項 (CPC1:期_議員_F など) を含む
# Rでは「*」や「:」を使うことで、自動的に2期〜4期のダミーとの交差項が作成されます。
model_formula <- ln_教育費 ~ 
  (CPC1 + CPC2 + CHPC1 + CHPC2) : 期_議員_F + 
  ln_算出決算総額

# PanelOLS (Two-way Fixed Effects)
# entity_effects=True, time_effects=True に相当
results <- plm(model_formula, 
               data = p_df, 
               model = "within", 
               effect = "twoways")

# 結果の表示（Clustered standard errors by entity）
# Pythonの cov_type='clustered' を再現
clustered_summary <- coeftest(results, vcov = vcovHC(results, type = "HC1", cluster = "group"))

print(clustered_summary)

# サマリー全体をテキスト出力
sink("regression_results_R.txt")
cat("--- Two-way Fixed Effects Regression Results ---\n")
print(summary(results))
cat("\n--- Clustered Standard Errors ---\n")
print(clustered_summary)
sink()

print("結果を regression_results_R.txt に保存しました！")
