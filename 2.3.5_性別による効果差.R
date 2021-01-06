#性別差による効果の差の検定
##ライブラリの読み込み
library(experimentdatar)
library(broom)
library(tidyverse)
data("vouchers")

##table4に使うデータを抜きだす
data_tbl4_bog95 = vouchers %>%
  filter(BOG95SMP == 1, TAB3SMPL == 1,
         !is.na(SCYFNSH), !is.na(FINISH6), !is.na(FINISH7), !is.na(FINISH8), 
         !is.na(PRSCH_C), !is.na(PRSCHA_1), !is.na(PRSCHA_2),
         !is.na(REPT), !is.na(REPT6), !is.na(NREPT), !is.na(INSCHL),
         !is.na(TOTSCYRS)
         ) %>%
  select(VOUCH0, 
         SVY, 
         HSVISIT, 
         DJAMUNDI, 
         PHONE, 
         AGE, 
         STRATA1:STRATA6, 
         STRATAMS,
         DBOGOTA, 
         D1993, 
         D1995, 
         D1997, 
         DMONTH1:DMONTH12, 
         SEX_MISS, 
         FINISH6:FINISH8,
         REPT6, 
         REPT, 
         NREPT, 
         SEX2, 
         TOTSCYRS, 
         MARRIED, 
         HASCHILD, 
         HOURSUM,
         WORKING3, 
         INSCHL, 
         PRSCH_C, 
         USNGSCH, 
         PRSCHA_1)

##女子生徒のみのデータ
regression_data_women = data_tbl4_bog95 %>% filter(SEX2 == 0)

##女子生徒のみのデータでまとめて回帰分析
df_models_women = models %>%
  mutate(model = map(.x = formula, .f = lm, data = regression_data_women)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))

##女子生徒モデルの結果を整形
df_results_women = df_models_women %>%
  mutate(formula = as.character(formula)) %>%
  mutate(formula, model_index, lm_result) %>%
  unnest(cols = c(lm_result))

##男子生徒のみのデータ
regression_data_men = data_tbl4_bog95 %>% filter(SEX2 == 1)

##男子生徒のみのデータでまとめて回帰分析
df_models_men = models %>%
  mutate(model = map(.x = formula, .f = lm, data = regression_data_men)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))

##男子生徒モデルの結果を整形
df_results_men = df_models_men %>%
  mutate(formula = as.character(formula)) %>%
  mutate(formula, model_index, lm_result) %>%
  unnest(cols = c(lm_result))
