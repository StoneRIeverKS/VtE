#1. ライブラリの読み込み
library("tidyverse")

#2. 乱数の固定
set.seed(20201024)

#3. バイアスのあるデータの作成
##バイアスの条件
###(history>300)|(recency<6)|(channel=="Multichannel")を購買意欲が高い人とみなす
biased_data = male_df %>%
  mutate(t_group=if_else(((history>300)|(recency<3)|(channel=="Multichannel")), 1, 0.5), #購買意欲が高い人を残す
         c_group=if_else(!((history>300)|(recency<3)|(channel=="Multichannel")), 1, 0.5), #購買意欲が低い人を残す
         random_number = runif(NROW(male_df), 0, 1)
  ) %>%
  filter(((t_group>random_number)&(treatment==1))|
         ((c_group>random_number)&(treatment==0))
  )

#4. treatmentについてグループ分けし、購入の発生確率と購入額の平均を求める
biased_summary_by_segment = biased_data %>%
  group_by(treatment) %>%
  summarise(conversion_rate=mean(conversion),
            spend_mean=mean(spend),
            count=n()
  )

#5. t検定を行う
##データの準備
###treatment=1のデータのspend列を取り出す
mens_mail_biased = biased_data %>%
  filter(treatment==1) %>%
  pull(spend)

###treatment=0のデータのspend列を取り出す
no_mail_biased = biased_data %>%
  filter(treatment==0) %>%
  pull(spend)

###t検定を行う
rct_ttest_biased = t.test(mens_mail_biased, no_mail_biased, var.equal=TRUE)