# 會用到的套件
packages <- c("haven", "dplyr")

# 安裝套件
install.packages(packages[!(packages %in% installed.packages())])

# 載入套件
loaded_packages <- sapply(packages, require, character.only = TRUE)

# 讀取資料庫
d <- read_spss("2019.sav")

##### 資料萃取 #####
# 提取人口變項
d1.1 <- d %>%
  select(starts_with(c("a1", "ra2", "ra9", "o4")), id) %>%
  mutate(o4 = ifelse(o4 == 98, NA, o4)) %>%
  mutate(o4 = ifelse(is.na(o4), mean(o4, na.rm = TRUE), o4))

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

# 提取假新聞風險感知
d2 <- d %>%
  select(starts_with(c("i7a", "i7b", "i7c")), id) %>%
  mutate_all(~ ifelse(. %in% c(97, 98), NA, .)) %>%
  mutate_at(vars(starts_with("i7")), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

# 提取新聞媒體可信度
d3 <- d %>%
  select(starts_with("i4."), id) %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

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
  select(id, np_use, web_use, TV_use)

# 提取數位技巧
d5 <- d %>%
  select(starts_with("h5"), id) %>%
  mutate_all(~ ifelse(is.na(.), 1, .)) %>%
  select(-h5.5, -h5.6, -h5.7, -h5.8)

# 合併檔案
all <- left_join(d1.1, d1, by = "id") %>%
  left_join(d2, by = "id") %>%
  left_join(d3, by = "id") %>%
  left_join(d4, by = "id") %>%
  left_join(d5, by = "id")

# 重新命名
names(all)[1:4] <- c("gender", "age", "edu", "salary")
names(all)[7:9] <- c("prevalence", "severity", "susceptibility")

# 新增變數
all <- all %>%
  mutate(med_cre = rowMeans(select(all, starts_with("i4."))),
         digital = rowMeans(select(all, starts_with("h5."))))

# 對數轉換
all <- all %>%
  mutate(TV_use_log = log(TV_use + 1),
         web_use_log = log(web_use + 1),
         np_use_log = log(np_use + 1))
