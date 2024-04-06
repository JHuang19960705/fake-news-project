install.packages("ggplot2")
library("ggplot2")

# 提取新聞媒體可信度
# 平均值差補
d3_na_mean <- d %>%
  select(starts_with("i4."), id) %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

# 留下遺漏值
d3_na <- d %>%
  select(starts_with("i4."), id)


# 資料處理
# 轉換數據為長格式
d3_long <- tidyr::pivot_longer(d3_na, cols = starts_with("i4."), names_to = "variable", values_to = "value")

# 資料視覺化
# 箱形圖
ggplot(data = d3_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9), color = "black") +
  labs(x = "媒體類型", y = "可信度", title = "不同新聞媒體可信度分佈比較", fill = "媒體類型") +
  scale_fill_manual(values = c("i4.1" = "blue", "i4.2" = "red", "i4.3" = "green", "i4.4" = "purple", "i4.5" = "orange"),
                    labels = c("電視", "報紙", "廣播", "雜誌", "網路")) +
  scale_x_discrete(labels = c("電視", "報紙", "廣播", "雜誌", "網路"))


# 小提琴圖
ggplot(data = d3_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin() +
  labs(x = "媒體類型", y = "可信度", title = "不同新聞媒體可信度分佈比較", fill = "媒體類型") +
  scale_fill_manual(values = c("i4.1" = "blue", "i4.2" = "red", "i4.3" = "green", "i4.4" = "purple", "i4.5" = "orange"),
                    labels = c("電視", "報紙", "廣播", "雜誌", "網路")) +
  scale_x_discrete(labels = c("電視", "報紙", "廣播", "雜誌", "網路")) +
  theme_minimal()

# 密度圖
ggplot(data = d3_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  labs(x = "可信度", y = "密度", title = "不同新聞媒體可信度分佈比較", fill = "媒體類型") +
  scale_fill_manual(values = c("i4.1" = "blue", "i4.2" = "red", "i4.3" = "green", "i4.4" = "purple", "i4.5" = "orange"),
                    labels = c("電視", "報紙", "廣播", "雜誌", "網路")) +
  theme_minimal()
