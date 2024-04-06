# 數據觀察
# 查看資料框結構資訊
str(d5)

# 獲取資料框每個欄位的摘要統計資訊
summary(d5)

table(d5)

unique(d5)

nrow(d5)

attr(d5, "label")

# 检查整个数据框是否存在缺失值
any(is.na(d5))

# 查看最大值
max_value <- max(d5, na.rm = TRUE)
print(max_value)


