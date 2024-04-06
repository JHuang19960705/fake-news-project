install.packages("ggplot2")
library("ggplot2")

# 提取假新聞暴露
d1 <- d %>%
  select(starts_with("i12.1"), id) %>%
  mutate(i12.1 = factor(i12.1),
         fknews_expose = case_when(
           i12.1 == "1" ~ 1,
           i12.1 == "2" ~ 0,
           i12.1 == "3" ~ 0
         )) %>%
  select(-i12.1)

# 創建柱狀圖
ggplot(d1, aes(x = factor(fknews_expose))) +
  geom_bar(stat = "count") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +  # 在柱子上顯示數字
  labs(x = "fknews_expose", y = "計數", title = "fknews_expose分佈")
