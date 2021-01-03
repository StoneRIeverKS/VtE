#ライブラリ(broom)の読み込み
library("broom")
library("tidyverse")

#visitを共変量に含めた回帰モデル
formula_vec_visit = c(spend ~ treatment + recency + channel + history + visit,
                      visit ~ treatment + recency + channel + history
                      )

#モデルをデータフレームに変換する
models_visit = enframe(formula_vec_visit, value = "formula")

#各モデルに対して回帰モデルを実行
df_models_visit = models_visit %>%
  mutate(models = map(.x = formula, .f = lm, data = biased_data)) %>%
  mutate(coef = map(.x = models, .f = tidy)) %>%
  mutate(formula = as.character(formula)) %>%
  unnest(cols = c(coef))

#OVBに必要な回帰係数を取り出す
alpha = df_models_visit %>%
  filter(formula == "spend ~ treatment + recency + channel + history + visit",
           term == "visit") %>%
  pull(estimate)
  
beta = df_models_visit %>%
  filter(formula == "visit ~ treatment + recency + channel + history",
         term == "treatment") %>%
  pull(estimate)

#OVBの値
alpha*beta

#visitを入れた回帰モデルを用いて効果を測定した結果
tau = df_models_visit %>%
  filter(formula == "spend ~ treatment + recency + channel + history + visit",
         term == "treatment") %>%
  pull(estimate)
