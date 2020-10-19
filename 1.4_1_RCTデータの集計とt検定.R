#tidyverseパッケージのインストール
#実際はtidyverseパッケージに含まれるdplyrパッケージを利用する
install.packages("tidyverse")

#libraryの呼び出し
library("tidyverse")

#データの読み込み
email_data = read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

#女性向けデータの削除
male_df = email_data %>%
  filter(segment!="Womens E-Mail") %>%
  mutate(treatment = if_else(segment=="Mens E-Mail", 1, 0))

#treatmentについてグループ分けし、購入の発生確率と購入額の平均を求める
summary_by_segment = male_df %>%
  group_by(treatment) %>%
  summarise(conversion_rate=mean(conversion),
            spend_mean=mean(spend),
            count=n()
            )

#t検定を行う
##データの準備
###treatment=1のデータのspend列を取り出す
mens_mail = male_df %>%
  filter(treatment==1) %>%
  pull(spend)

###treatment=0のデータのspend列を取り出す
no_mail = male_df %>%
  filter(treatment==0) %>%
  pull(spend)

###t検定を行う
rct_ttest = t.test(mens_mail, no_mail, var.equal=TRUE)



