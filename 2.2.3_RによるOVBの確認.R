#ライブラリ(broom)の読み込み
library("broom")
library("tidyverse")

#モデル式のベクトル
formula_vec = c(spend ~ treatment + recency + channel, #モデルA
                spend ~ treatment + recency + channel + history, #モデルB
                history ~ treatment + channel + recency) #モデルC

#formula_vecに名前をつける
names(formula_vec) = paste("reg", LETTERS[1:3], sep="_")

#モデルをデータフレームに変換する
models = enframe(formula_vec, name = "model_index", value = "formula")

#まとめて回帰分析を実行する
df_models = models %>%
  mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))

#モデルの結果を整形
df_results = df_models %>%
  mutate(formula = as.character(formula)) %>%
  select(formula, model_index, lm_result) %>%
  unnest(cols = c(lm_result))

#各モデルから介入変数(treatment)の回帰係数を取り出す
treatment_coef = df_results %>%
  filter(term == "treatment") %>%
  select(model_index, estimate)

#モデルBから共変量(history)の回帰係数を取り出す
history_coef = df_results %>%
  filter(model_index == "reg_B",
         term == "history") %>%
  select(model_index, estimate)

#OVBの確認
##alpha1
alpha1 = treatment_coef %>%
  filter(model_index == "reg_A") %>%
  pull(estimate)

##beta1
beta1 = treatment_coef %>%
  filter(model_index == "reg_B") %>%
  pull(estimate)

##OVB1 = alpha1-beta1
OVB1 = alpha1-beta1

##gamma1
gamma1 = treatment_coef %>%
    filter(model_index == "reg_C") %>%
  pull(estimate)

##beta4
beta4 = history_coef["estimate"]

##OVB2 = gamma1*beta4
OVB2 = gamma1*beta4
