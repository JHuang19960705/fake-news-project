install.packages("ggplot2")
library("ggplot2")

# 提取假新聞風險感知
d2 <- d %>%
  select(starts_with(c("i7a", "i7b", "i7c")), id) %>%
  mutate_all(~ ifelse(. %in% c(97, 98), NA, .)) %>%
  mutate_at(vars(starts_with("i7")), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

# 資料處理
# i7a：普遍性
d2$i7a_group <- cut(d2$i7a, breaks = c(0, 1, 2, 3, 3.9, 4, 6),
                    labels = c("非常不普遍", "不普遍", "普通", "未填", "普遍", "非常普遍"))

# i7b：嚴重性
d2$i7b_group <- cut(d2$i7b, breaks = c(0, 1, 2, 3, 3.9, 4, 6),
                    labels = c("非常不嚴重", "不嚴重", "普通", "未填", "嚴重", "非常嚴重"))

# i7c：易感性
d2$i7c_group <- cut(d2$i7c, breaks = c(0, 1, 2, 2.9, 3, 4, 6),
                    labels = c("非常不可能", "不可能", "未填", "普通", "可能", "非常可能"))


# 資料視覺化
# i7a_group：普遍性
ggplot(d2, aes(x = i7a_group)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = 'black') +
  labs(x = "普遍性", y = "人數", title = "普遍性分佈")


# i7b_group：嚴重性
ggplot(d2, aes(x = i7b_group)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = 'black') +
  labs(x = "嚴重性", y = "人數", title = "嚴重性分佈")


# i7c_group：易感性
ggplot(d2, aes(x = i7c_group)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = 'black') +
  labs(x = "易感性", y = "人數", title = "易感性分佈")
