#ライブラリとデータセットの読み込み
#install.packages("remotes") #最初だけ実行が必要
remotes::install_github("itamarcaspi/experimentdatar") #remotesパッケージ内のinstall_github関数でgithub上のパッケージを読み込んだ
library(experimentdatar)
library(broom)
library(tidyverse)
data("vouchers")

#回帰式の準備
##介入変数
formula_x_base = "VOUCH0"
##共変量
formula_x_covariate = "SVY + HSVISIT + AGE + STRATA1 + STRATA2 + STRATA3 + STRATA4 +
                       STRATA5 + STRATA6 + STRATAMS + D1993 + D1995 + D1997 + DMONTH1 + 
                       DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + DMONTH6 + DMONTH7 + 
                       DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11 + DMONTH12 + SEX2"
##目的変数
formula_y = c("TOTSCYRS",
              "INSCHL",
              "PRSCH_C",
              "USNGSCH",
              "PRSCHA_1",
              "FINISH6",
              "FINISH7",
              "FINISH8",
              "REPT",
              "NREPT",
              "MARRIED",
              "HASCHILD",
              "HOURSUM",
              "WORKING",
              "REPT6")

##共変量を含まない回帰式
base_reg_formula = paste(formula_y, formula_x_base, sep = "~")
names(base_reg_formula) = paste(formula_y, "base", sep = "_")

##共変量を含む回帰式
covariate_reg_formula = paste(base_reg_formula, formula_x_covariate, sep = "+")
names(covariate_reg_formula) = paste(formula_y, "covariate", sep = "_")

##モデル式のベクトルを作成
table3_formula = c(base_reg_formula, covariate_reg_formula)

##モデル式のデータをデータフレーム化する
models = table3_formula %>%
  enframe(name = "model_index", value = "formula")

#回帰分析を実行
##bogota 1995のデータを抽出
regression_data = vouchers %>%
  filter(TAB3SMPL == 1, BOG95SMP == 1)

##まとめて回帰分析を実行
df_models = models %>%
  mutate(model = map(.x = formula, .f = lm, data = regression_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))

##モデルの結果を整形
df_results = df_models %>%
  mutate(formula = as.character(formula)) %>%
  mutate(formula, model_index, lm_result) %>%
  unnest(cols = c(lm_result))

#回帰分析の結果を確認
using_voucher_results = df_results %>%
  filter(term == "VOUCH0", str_detect(model_index, "PRSCHA_1|USNGSCH")) %>%
  select(model_index, term, estimate, std.error, p.value) %>%
  arrange(model_index)




