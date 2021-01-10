#留年と進級の傾向を箱ひげ図で表示する

##wdの設定
wd = "C:\\Users\\stone\\Documents\\統計\\効果検証入門\\code"
setwd(wd)
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
  
##図2.4の作成
ryunen_sinkyu = need_voucher_results %>%
  ggplot(mapping = aes(x = model_index, y = estimate)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm"))

##グラフの保存
ggsave(file = "ryunen_sinkyu.png", plot = ryunen_sinkyu)


  
    
