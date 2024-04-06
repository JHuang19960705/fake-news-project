install.packages("ggplot2")
library("ggplot2")

# 提取數位技巧
d5 <- d %>%
  select(starts_with("h5"), id) %>%
  select(-h5.5, -h5.6, -h5.7, -h5.8) %>%
  mutate(across(starts_with("h5"), ~ ifelse(is.na(.), 1, .)))

# 轉換數據為長格式
d5_long <- tidyr::pivot_longer(d5, cols = c(h5.1, h5.2, h5.3, h5.4), names_to = "digital_type", values_to = "digital_skill")

# 箱線圖
ggplot(d5_long, aes(x = digital_type, y = digital_skill)) +
  geom_boxplot() +
  labs(x = "數位使用類型", y = "數位技巧", title = "不同媒體使用時間分佈") +
  scale_x_discrete(labels = c("電子郵件", "上網瀏覽", "傳輸資料", "看影片")) +  # 修改標籤名稱
  theme_minimal()

# 數據可視化 - 小提琴圖
ggplot(data = d5_long, aes(x = digital_type, y = digital_skill, fill = digital_type)) +
  geom_violin() +
  labs(x = "數位使用類型", y = "數位技巧", title = "不同新聞媒體使用時間分佈比較", fill = "數位使用類型") +
  scale_fill_manual(values = c("blue", "red", "green", "yellow"),
                    labels = c("電子郵件", "上網瀏覽", "傳輸資料", "看影片")) + # 修改圖例名稱
  scale_x_discrete(labels = c("電子郵件", "上網瀏覽", "傳輸資料", "看影片")) +  # 修改標籤名稱
  theme_minimal()

# 數據可視化 - 密度圖
ggplot(data = d5_long, aes(x = digital_skill, fill = digital_type)) +
  geom_density(alpha = 0.5) +
  labs(x = "數位技巧", y = "密度", title = "不同新聞媒體使用時間分佈比較", fill = "數位使用類型") +
  scale_fill_manual(values = c("blue", "red", "green", "yellow"),
                    labels = c("電子郵件", "上網瀏覽", "傳輸資料", "看影片")) + # 修改圖例名稱
  theme_minimal()
