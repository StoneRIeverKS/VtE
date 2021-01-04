#留年と進級の傾向を箱ひげ図で表示する

##ggplot2のインストールと読み込み
install.packages("ggplot2")
library("ggplot2")
library(broom)
library(tidyverse)

##データの整形
###必要な目的変数
need_y = "FINISH6_covariate|FINISH7_covariate|FINISH8_covariate|INSCHL_covariate|NREPT_covariate|PRSCH_C_covariate|REPT_covariate|REPT6_covariate"
###必要な推定量と標準偏差
need_voucher_results = df_results %>%
  filter(term == "VOUCH0", str_detect(model_index,need_y)) %>%
  select(model_index, estimate, std.error) %>%
  mutate(upper = estimate+1.96*std.error, lower = estimate-1.96*std.error) %>%
  arrange(model_index)
  
##グラフの描画
ryunen_sinkyu = ggplot(data = need_voucher_results, mapping = aes(x = model_index, y = estimate)) + 
  geom_point() +
  geom_hline(yintercept=0)

https://qiita.com/Hiroyuki1993/items/5a1a3331e5c8c79d9c9d
