#回帰分析の実行
#目的変数(spend:購入額)、介入変数(treatment:メールは配信有無)
##モデル1→共変量(history:過去の購入額)
##モデル2→共変量(history、recency:最後の購入、channel:接触チャネルが複数あるか否か)
biased_reg_1 = lm(data = biased_data,
                formula = spend ~ treatment + history)

biased_reg_2 = lm(data = biased_data,
                  formula = spend ~ treatment + history + recency + channel)

#分析結果のレポート
summary(biased_reg_1)
summary(biased_reg_2)
#ライブラリ(broom)の読み出し
library(broom)

#推定されたパラメータだけを取り出す
biased_reg_coef_1 = tidy(biased_reg_1)
biased_reg_coef_2 = tidy(biased_reg_2)

