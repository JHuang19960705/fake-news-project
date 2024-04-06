install.packages("ggplot2")
library("ggplot2")

# 提取新聞使用
d4 <- d %>%
  select(starts_with(c("c1a", "c1b", "c1c", "c3a", "c3b", "c3c", "f1", "f2", "f3", "f4", "f7")), id) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(np_time = c1b.1 * 60 + c1b.2,
         np_use = np_time * c1a,
         web_time = c3b.1 * 60 + c3b.2,
         web_use = web_time * c3a,
         TV_wk_time = f2.1 * 60 + f2.2,
         TV_wk_use = TV_wk_time * f1,
         TV_wkend_time = f4.1 * 60 + f4.2,
         TV_wkend_use = TV_wkend_time * f3,
         TV_use = TV_wk_use + TV_wkend_use) %>%
  # 對數轉換
  mutate(TV_use_log = log(TV_use + 1), 
         web_use_log = log(web_use + 1),
         np_use_log = log(np_use + 1)) %>%
  select(id, np_use, web_use, TV_use, TV_use_log, web_use_log, np_use_log)

# 轉換數據為長格式
d4_long <- tidyr::pivot_longer(d4, cols = c(np_use_log, web_use_log, TV_use_log), names_to = "media_type", values_to = "usage_time")

# 箱線圖
ggplot(d4_long, aes(x = media_type, y = usage_time)) +
  geom_boxplot() +
  labs(x = "媒體類型", y = "使用時間(log值)", title = "不同媒體使用時間分佈") +
  scale_x_discrete(labels = c("報紙", "電視", "網路")) +
  theme_minimal()

# 數據可視化 - 小提琴圖
ggplot(data = d4_long, aes(x = media_type, y = usage_time, fill = media_type)) +
  geom_violin() +
  labs(x = "媒體類型", y = "使用時間(log值)", title = "不同新聞媒體使用時間分佈比較", fill = "媒體類型") +
  scale_fill_manual(values = c("blue", "red", "green"),
                    labels = c("電視", "網路", "報紙")) +  # 修改圖例名稱
  scale_x_discrete(labels = c("電視", "網路", "報紙")) +  # 修改標籤名稱
  theme_minimal()

# 數據可視化 - 密度圖
ggplot(data = d4_long, aes(x = usage_time, fill = media_type)) +
  geom_density(alpha = 0.5) +
  labs(x = "使用時間(log值)", y = "密度", title = "不同新聞媒體使用時間分佈比較", fill = "媒體類型") +
  scale_fill_manual(values = c("blue", "red", "green"),
                    labels = c("電視", "網路", "報紙")) +  # 修改圖例名稱
  theme_minimal()
