# ==========================================
# 1. ライブラリの読み込み
# ==========================================
library(tidyverse)   # データ操作・可視化 (ggplot2, dplyr, tidyrなど)
library(readxl)      # Excel読み込み
library(psych)       # 偏相関係数 (pcor)
library(corrplot)    # 相関行列の可視化
library(GGally)      # ペアプロット (ggpairs)
library(stats)       # PCA (prcomp)
library(plm)         # パネルデータ分析 (PanelOLSの代替)
library(ggrepel)     # グラフ内のラベル重なり防止

# ==========================================
# 2. データの読み取りと加工
# ==========================================
# ファイルパスは適宜調整してください
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
    年 = format(日付, "%Y") %>% as.numeric()
  )

# 議員と首長の分離
counciler_df <- merged_df %>% filter(種別 == "議員")
chief_df <- merged_df %>% filter(種別 == "首長")

# ==========================================
# 3. 可視化
# ==========================================

# ヒストグラム（グリッド表示）
counciler_df %>%
  select(年齢中央値, 年齢最小値, `65歳以上割合`, 新人割合, `自民+公明割合`, `共産+社民割合`, 無所属割合) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

# ペアプロット
# ggpairs(counciler_df %>% select(年齢中央値, 年齢最小値, `65歳以上割合`, 新人割合, `自民+公明割合`, `共産+社民割合`, 無所属割合))

# 相関行列
cor_matrix <- counciler_df %>%
  select(年齢中央値, 年齢最小値, `65歳以上割合`, 新人割合, `自民+公明割合`, `共産+社民割合`, 無所属割合) %>%
  cor(use = "complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", type = "upper")

# 軌跡グラフの関数定義 (Pythonのplot_temporal_trajectoryに相当)
plot_temporal_trajectory <- function(data, x_col, y_col, group_col, time_col, title = "") {
  ggplot(data, aes_string(x = x_col, y = y_col, group = group_col, color = time_col)) +
    geom_path(arrow = arrow(length = unit(0.2, "cm")), color = "gray", alpha = 0.6) +
    geom_point(size = 2) +
    geom_text_repel(data = data %>% group_by(.data[[group_col]]) %>% filter(.data[[time_col]] == max(.data[[time_col]])),
                    aes_string(label = group_col), size = 3) +
    scale_color_viridis_c() +
    labs(title = title) +
    theme_minimal()
}

# ==========================================
# 4. 欠損値補完 (男女比率)
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
# 5. 主成分分析 (PCA)
# ==========================================

# --- 議員 PCA ---
pca_input_c <- counciler_df %>%
  mutate(`65pp` = `65歳以上割合` - `65歳以上人口割合`) %>%
  select(`65pp`, 新人割合, 男女比率, `自民+公明割合`, `共産+社民割合`, 無所属割合) %>%
  drop_na()

pca_res_c <- prcomp(pca_input_c, scale. = TRUE)

# 寄与率と負荷量
summary(pca_res_c)
print(pca_res_c$rotation) # Loadings

# 元のデータにPCAスコアを結合
pca_scores_c <- as.data.frame(pca_res_c$x[, 1:2]) %>%
  rename(CPC1 = PC1, CPC2 = PC2) %>%
  bind_cols(counciler_df[rownames(pca_input_c), c("市区町村", "年")])

# --- 首長 PCA ---
pca_input_ch <- chief_df %>%
  select(年齢最小値, 新人割合, 男性, `自民+公明割合`, `共産+社民割合`, 無所属割合) %>%
  drop_na()

pca_res_ch <- prcomp(pca_input_ch, scale. = TRUE)
pca_scores_ch <- as.data.frame(pca_res_ch$x[, 1:2]) %>%
  rename(CHPC1 = PC1, CHPC2 = PC2) %>%
  bind_cols(chief_df[rownames(pca_input_ch), c("市区町村", "年")])

