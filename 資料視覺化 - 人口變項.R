install.packages("ggplot2")
library("ggplot2")

# 提取人口變項
d1.1 <- d %>%
  select(starts_with(c("a1", "ra2", "ra9", "o4")), id) %>%
  mutate(o4 = ifelse(o4 == 98, NA, o4)) %>%
  mutate(o4 = ifelse(is.na(o4), mean(o4, na.rm = TRUE), o4))

# 資料處理
# ra2：年齡
d1.1$age_group <- cut(d1.1$ra2, breaks = c(18, 30, 40, 50, 60, 70, 100),
                      labels = c("18 歲到 29 歲", "30 歲到 39 歲", "40 歲到 49 歲",
                                 "50 歲到 59 歲", "60 歲到 69 歲", "70 歲及以上"))
# o4：薪資
d1.1$salary_group <- cut(d1.1$o4, breaks = c(0, 3, 4, 4.8, 5, 6, 7, 24),
                      labels = c("2 萬元以下", "2 萬元以上\n至 3 萬元", "未答", "3 萬元以上\n至 4 萬元",
                                 "4 萬元以上\n至 5 萬元", "5 萬元以上\n至 6 萬元", "6 萬元以上"))

# 資料視覺化
# a1：性別
ggplot(d1.1, aes(x = factor(a1))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = 'black') +
  labs(x = "性別", y = "人數", title = "性別分佈") +
  scale_x_discrete(labels = c("男", "女"))

# age_group：年齡
ggplot(d1.1, aes(x = age_group)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = 'black') +
  labs(x = "年齡", y = "人數", title = "年齡分佈")

# ra9：教育程度分層
ggplot(d1.1, aes(x = factor(ra9))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = 'black') +
  labs(x = "教育程度", y = "人數", title = "教育程度分佈") +
  scale_x_discrete(labels = c("不識字", "自修/小學", "國中(初)中/初職", "高中普通科/\n高中職業科/\n高職/士官學校", "專科/大學/\n碩士/博士"))

# o4：薪資分布
ggplot(d1.1, aes(x = salary_group)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = 'black') +
  labs(x = "薪資", y = "人數", title = "薪資分佈")

