import numpy as np
import pandas as pd
import unicodedata

def clean_column_name(name):
    """カラム名の全角半角・記号の揺れをきれいにする"""
    if not isinstance(name, str):
        return name
    name = unicodedata.normalize('NFKC', name)
    name = name.replace('，', '、').strip()
    name = name.replace(',', '、').strip()
    return name

# 表記揺れの対応リスト
rename_dict = {
    '人口総数': '総人口',
    '人口総数（男）': '総人口（男）',
    '人口総数（女）': '総人口（女）',
    '事業所総数': '事業所数',
    '保育所等数（基本票）': '保育所等数'
}

# 読み込みと前処理
all_dfs = []
for i in range(2018, 2026):
    df_temp = pd.read_excel(f"data/SSDSE-A-{i}.xlsx", skiprows=1, header=[0, 1])
    
    # 地域コードの上の列を「Region_Code」に書き換える
    df_temp.columns = pd.MultiIndex.from_tuples([
        ('Region_Code' if col[1] == '地域コード' else col[0], col[1]) 
        for col in df_temp.columns
    ])
    all_dfs.append(df_temp)

y_df = pd.concat(all_dfs, ignore_index=True) 


# データの変形（縦持ち化）とクレンジング
df_final = (
    y_df.drop(columns=[('年度', '都道府県'), ('年度', '市区町村')])
        .set_index([('Region_Code', '地域コード')])
        .stack(level=0)
        .reset_index()
        .rename(columns={'level_1': '年度', ('Region_Code', '地域コード'): '地域コード'})
)

# 不要な 'Unnamed' 行の除外
df_final = df_final[df_final['年度'] != 'Unnamed: 1_level_0']

# 行の統合（地域コードと年度が重複している行をまとめる）
df_final = df_final.groupby(['地域コード', '年度'], as_index=False).first()



# カラム名の統一と重複列の統合（上記の関数を利用）
df_final.columns = [rename_dict.get(clean_column_name(col), clean_column_name(col)) for col in df_final.columns]
df_final = df_final.groupby(df_final.columns, axis=1).first()


# 列の並べ替え
first_cols = ['地域コード', '年度']
other_cols = [col for col in df_final.columns if col not in first_cols]
df_final = df_final[first_cols + other_cols].sort_values(['地域コード', '年度']).reset_index(drop=True)


# 出力
df_final.to_csv('data/local_gov_statistics.csv', index=False)



#　地域コードと都道府県、市区町村の対応表を作成
selected_cols = [('Region_Code', '地域コード'), ('年度', '都道府県'), ('年度', '市区町村')]
y_correspond = y_df.loc[:, selected_cols].copy()
y_correspond.columns = y_correspond.columns.droplevel(0)

y_correspond = (
    y_correspond.drop_duplicates(subset=['地域コード'])
                .sort_values('地域コード')
                .reset_index(drop=True)
)

# 出力
y_correspond.to_csv('data/region_code_correspondence.csv', index=False)