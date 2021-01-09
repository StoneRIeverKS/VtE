#性別差による効果の差の検定
wd = "C:\\Users\\stone\\Documents\\統計\\効果検証入門"
setwd(wd)
##ライブラリの読み込み
library(ggplot2)
library(gridExtra)
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

#グラフの描画
##グラフの描画
###推定結果に性別のフラグをつけて結合する
df_results_male = df_results_men %>%
  mutate(sex = "male")

df_results_female = df_results_women %>%
  mutate(sex = "female")
###結合
df_results_sexFlg = rbind(df_results_male, df_results_female)

###図2.5のグラフを作成
result_plot2.5 = df_results_sexFlg %>%
  filter(str_detect(model_index, "PRSCHA_1_covariate|USNGSCH_covariate"), term == "VOUCH0") %>%
  ggplot(mapping = aes(x = model_index, y = estimate)) + 
  geom_point() +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(sex~.) +
  labs(title = "通学傾向")

#グラフの保存
ggsave(file = "result_plot2_5.png", plot = result_plot2.5)


###図2.6のグラフ作成
y_str = "FINISH6_covariate|FINISH7_covariate|FINISH8_covariate|INSCHL_covariate|NREPT_covariate|PRSCH_C_covariate|REPT_covariate|REPT6_covariate|TOTSCYRS_covariate"
result_plot2.6 = df_results_sexFlg %>%
  filter(str_detect(model_index, y_str), term == "VOUCH0") %>%
  ggplot(mapping = aes(x = model_index, y = estimate)) + 
  geom_point() +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(sex~.)
#グラフの保存
ggsave(file = "result_plot2_6.png", plot = result_plot2.6)

#図2.7を作成
result_plot2.7 = df_results_sexFlg %>%
  filter(model_index == "HOURSUM_covariate", term == "VOUCH0") %>%
  ggplot(mapping = aes(x = model_index, y = estimate)) + 
  geom_point() +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(~sex)

#グラフの保存
ggsave(file = "result_plot2_7.png", plot = result_plot2.7)
