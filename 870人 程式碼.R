# 會用到的套件
packages <- c("car", "psych", "lm.beta", "lavaan", "ggplot2")

# 安裝套件
install.packages(packages[!(packages %in% installed.packages())])

# 載入套件
loaded_packages <- sapply(packages, require, character.only = TRUE)

# 讀取資料庫
d <- read_spss("2019.sav")

#####################
##### 選擇性接觸計算 #####
#撈取資料
v1=c("c1a","c1b","c1c","c3a","c3b","c3c","f1","f2","f3","f4","f7")
d_sele_exp=select(d,starts_with(v1),id,l12,l13,l14,l15)

d_sele_exp$l12[d_sele_exp$l12==97] = NA
d_sele_exp$l12[d_sele_exp$l12==98] = NA
d_sele_exp$l13[d_sele_exp$l13==97] = NA
d_sele_exp$l13[d_sele_exp$l13==98] = NA
d_sele_exp$l14[d_sele_exp$l14==97] = NA
d_sele_exp$l14[d_sele_exp$l14==98] = NA
d_sele_exp[is.na(d_sele_exp)]=0
##### step1: np, web & TV total exposure #####
d_sele_exp=d_sele_exp%>%
  mutate(np_time=c1b.1*60+c1b.2)%>%
  mutate(np_expose=np_time*c1a)%>%
  mutate(web_time=c3b.1*60+c3b.2)%>%
  mutate(web_expose=web_time*c3a) %>%
  mutate(TV_wk_time=f2.1*60+f2.2)%>%
  mutate(TV_wk_expose=TV_wk_time*f1)%>%
  mutate(TV_wkend_time=f4.1*60+f4.2)%>%
  mutate(TV_wkend_expose=TV_wkend_time*f3)%>%
  mutate(TV_expose=TV_wk_expose+TV_wkend_expose)

# 檢視資料
names(d_sele_exp)

# 計算各欄位的平均值
colMeans(d_sele_exp[c("np_expose", "web_expose", "TV_expose")])

# 計算各欄位的標準差
apply(d_sele_exp[c("np_expose", "web_expose", "TV_expose")], 2, sd)

##### step2: 媒體分黨派 #####
# calculate channel
# 製作該使用者報紙、網路的政黨傾向
d_sele_exp=d_sele_exp%>%
  mutate(blue_np=c1c.3+c1c.4+c1c.7+c1c.10+c1c.13+c1c.16+c1c.17+c1c.20+c1c.21+c1c.22+c1c.38)%>%
  mutate(neutral_np=c1c.2+c1c.5+c1c.6+c1c.12+c1c.14+c1c.15+c1c.26+c1c.28+c1c.30+c1c.31+c1c.32+c1c.33+c1c.34+c1c.35+c1c.37+c1c.40+c1c.42)%>%
  mutate(green_np=c1c.1+c1c.11+c1c.19+c1c.36+c1c.43)%>%
  mutate(blue_web=c3c.1+c3c.2+c3c.36+c3c.41+c3c.18+c3c.34+c3c.35+c3c.21)%>%
  mutate(neutral_web=c3c.24+c3c.28+c3c.10+c3c.9+c3c.33+c3c.43+c3c.44+c3c.30+c3c.29+c3c.7+c3c.26+c3c.6+c3c.8+c3c.40+c3c.39+c3c.19+c3c.23+c3c.25+c3c.32+c3c.16+c3c.13+c3c.22+c3c.15+c3c.14+c3c.20+c3c.12)%>%
  mutate(green_web=c3c.4+c3c.38+c3c.17+c3c.37+c3c.38+c3c.42)

# 列印列名稱
names(d_sele_exp)

# 定義要計算的變數
variables <- c("blue_np", "blue_web", "neutral_np", "neutral_web", "green_np", "green_web")

# 計算每個變數的平均值和標準差，並產生摘要統計
for (variable in variables) {
  cat("Variable:", variable, "\n")
  cat("Mean:", mean(d_sele_exp[[variable]]), "\n")
  cat("Standard deviation:", sd(d_sele_exp[[variable]]), "\n")
  cat("Summary:\n")
  print(summary(d_sele_exp[[variable]]))
  cat("\n")
}

# 製作該使用者電視政黨傾向
# 檢視數據
table(d_sele_exp$f7)
summary(d_sele_exp$f7)
is.na(d_sele_exp$f7)

# 轉成字串
d_sele_exp$f7=as.character(d_sele_exp$f7)

# 幫電視台分配政黨傾向
d_sele_exp <- d_sele_exp %>%
  mutate(tv_channel = case_when(
    f7 %in% c("0", "888") ~ 0,
    f7 %in% c("4", "5", "12", "18", "28", "119", "7", "14", "29", "21", "1", "101", "105", "51") ~ 1,
    f7 %in% c("8", "11", "15", "16", "19", "20", "22", "23", "24", "25", "26", "27", "30", "31", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100", "102", "103", "104", "106", "109", "110", "111", "112", "113", "114", "117", "118", "120", "121", "122", "123", "124", "125", "126", "127", "128", "129", "130", "131", "132", "133", "134", "135", "136", "137", "138", "139", "140", "141", "142", "144") ~ 2,
    f7 %in% c("6", "13", "3", "116", "9", "52", "32", "2", "10", "17", "107", "108", "115", "143") ~ 3,
    TRUE ~ NA_integer_
  ))

# 檢查 tv_channel 變數的結果
table(d_sele_exp$tv_channel)

# 製作變數
d_sele_exp=d_sele_exp%>%
  mutate(blue_tv=ifelse(d_sele_exp$tv_channel==1,1,0))%>%
  mutate(neutral_tv=ifelse(d_sele_exp$tv_channel==2,1,0))%>%
  mutate(green_tv=ifelse(d_sele_exp$tv_channel==3,1,0))

# 檢視數據
names(d_sele_exp)
table(d_sele_exp$blue_tv)
table(d_sele_exp$neutral_tv)
table(d_sele_exp$green_tv)
class(d_sele_exp$blue_tv)
d_sele_exp[is.na(d_sele_exp)]=0

##### step3: 計算使用時間 #####
# calculate np time
d_sele_exp=d_sele_exp%>%
  mutate(total_np_channel=blue_np+neutral_np+green_np)%>%
  mutate(blue_np_time=(blue_np/total_np_channel)*np_expose)%>%
  mutate(neutral_np_time=(neutral_np/total_np_channel)*np_expose)%>%
  mutate(green_np_time=(green_np/total_np_channel)*np_expose)

# 使用向量化操作計算平均值
means <- colMeans(d_sele_exp[c("blue_np_time", "neutral_np_time", "green_np_time")], na.rm = TRUE)
print(means)

# 使用向量化操作計算標準差
sds <- apply(d_sele_exp[c("blue_np_time", "neutral_np_time", "green_np_time")], 2, sd, na.rm = TRUE)
print(sds)

# 檢查是否存在 NA 值
if(any(is.na(d_sele_exp))) {
  print("有 NA 值，已替換為 0 ")
  # 將 NA 值替換為 0
  d_sele_exp[is.na(d_sele_exp)] <- 0
}

# calculate web time
d_sele_exp=d_sele_exp%>%
  mutate(total_web_channel=blue_web+neutral_web+green_web)%>%
  mutate(blue_web_time=(blue_web/total_web_channel)*web_expose)%>%
  mutate(neutral_web_time=(neutral_web/total_web_channel)*web_expose)%>%
  mutate(green_web_time=(green_web/total_web_channel)*web_expose)

# 使用向量化操作計算平均值
means <- colMeans(d_sele_exp[c("blue_web_time", "neutral_web_time", "green_web_time")], na.rm = TRUE)
print(means)

# 使用向量化操作計算標準差
sds <- apply(d_sele_exp[c("blue_web_time", "neutral_web_time", "green_web_time")], 2, sd, na.rm = TRUE)
print(sds)

# 檢查是否存在 NA 值
if(any(is.na(d_sele_exp))) {
  print("有 NA 值，已替換為 0 ")
  # 將 NA 值替換為 0
  d_sele_exp[is.na(d_sele_exp)] <- 0
}

# calculate tv time
d_sele_exp=d_sele_exp%>%
  mutate(blue_tv_time=TV_expose*blue_tv)%>%
  mutate(neutral_tv_time=TV_expose*neutral_tv)%>%
  mutate(green_tv_time=TV_expose*green_tv)

# 使用向量化操作計算平均值
means <- colMeans(d_sele_exp[c("blue_tv_time", "neutral_tv_time", "green_tv_time")], na.rm = TRUE)
print(means)

# 使用向量化操作計算標準差
sds <- apply(d_sele_exp[c("blue_tv_time", "neutral_tv_time", "green_tv_time")], 2, sd, na.rm = TRUE)
print(sds)

# 檢查是否存在 NA 值
if(any(is.na(d_sele_exp))) {
  print("有 NA 值，已替換為 0 ")
  # 將 NA 值替換為 0
  d_sele_exp[is.na(d_sele_exp)] <- 0
}

##### step4: total three types of media #####
d_sele_exp=d_sele_exp%>%
  mutate(blue_total_time=blue_np_time+blue_web_time+blue_tv_time)%>%
  mutate(neutral_total_time=neutral_np_time+neutral_web_time+neutral_tv_time)%>%
  mutate(green_total_time=green_np_time+green_web_time+green_tv_time)%>%
  mutate(total_time=blue_total_time+neutral_total_time+green_total_time)

# 使用向量化操作計算平均值
means <- colMeans(d_sele_exp[c("blue_total_time", "neutral_total_time", "green_total_time")], na.rm = TRUE)
print(means)

# 使用向量化操作計算標準差
sds <- apply(d_sele_exp[c("blue_total_time", "neutral_total_time", "green_total_time")], 2, sd, na.rm = TRUE)
print(sds)

##### step5: compare with partisanship (only analyze people with partisanship) #####
d_sele_exp$l14=as.factor(d_sele_exp$l14)

dl14=d_sele_exp%>%
  mutate(party=recode(l14,"1"=1,"2"=3,"3"=1,"4"=1,"5"=3,"19"=3,.default = 2))

table(dl14$party)

dblue=dl14%>%
  filter(party==1)%>%
  mutate(sele_expo=(blue_total_time-green_total_time-neutral_total_time))

mean(dblue$sele_expo,na.rm = T)
mean(dblue$blue_total_time,na.rm = T)
mean(dblue$green_total_time,na.rm = T)
summary(dblue$sele_expo)
sd(dblue$sele_expo)

dgreen=dl14%>%
  filter(party==3)%>%
  mutate(sele_expo=green_total_time-blue_total_time-neutral_total_time)

mean(dgreen$sele_expo,na.rm = T)
mean(dgreen$blue_total_time,na.rm = T)
mean(dgreen$green_total_time,na.rm = T)
summary(dgreen$sele_expo)
sd(dgreen$sele_expo)

sele_exp=rbind(dblue,dgreen)
mean(sele_exp$sele_expo,na.rm = T)
summary(sele_exp)
sele_exp=select(sele_exp,sele_expo,id)

#########################
##### 選擇性接觸(870人)樣本描述性統計 #####
sele_exp=left_join(sele_exp,all,by="id")
names(sele_exp)
##### 重新變回遺漏值 #####
sele_exp$salary[sele_exp$salary==4.714868] = NA
sele_exp$prevalence[sele_exp$prevalence==3.78] = NA
sele_exp$severity[sele_exp$severity==3.89] = NA
sele_exp$susceptibility[sele_exp$susceptibility==2.76] = NA
##### 算平均值 #####
summary(sele_exp$salary)
summary(sele_exp$prevalence)
summary(sele_exp$severity)
summary(sele_exp$susceptibility)
##### 平均值插補 #####
sele_exp$salary[is.na(sele_exp$salary)] = 4.987
sele_exp$prevalence[is.na(sele_exp$prevalence)] = 3.848
sele_exp$severity[is.na(sele_exp$severity)] = 3.952
sele_exp$susceptibility[is.na(sele_exp$susceptibility)] = 2.788
##### 數位技巧計算 #####
sele_exp=sele_exp%>%
  mutate(digital=(h5.1+h5.2+h5.3+h5.4)/4)
#交乘項計算
##第一步：標準化
names(sele_exp)
table(sele_exp$fknews_expose)
sele_exp$fknews_expose=as.numeric(sele_exp$fknews_expose)
sele_exp$z_prevalence <- scale(sele_exp[,8])
sele_exp$z_severity <- scale(sele_exp[,9])
sele_exp$z_susceptibility <- scale(sele_exp[,10])
sele_exp$z_digital <- scale(sele_exp[,15])
##第二步：相乘
sele_exp=sele_exp%>%
  mutate(mod_fk_di=(fknews_expose*z_digital))%>%
  mutate(mod_prvlce_di=(z_prevalence*z_digital))%>%
  mutate(mod_svrty_di=(z_severity*z_digital))%>%
  mutate(mod_suseptblty_di=(z_susceptibility*z_digital))
sele_exp$fknews_expose=as.factor(sele_exp$fknews_expose)
save(sele_exp,file="sele_exp_1117.Rdata")
##### 信效度分析（870 人）#####
digital=select(sele_exp,h5.1,h5.2,h5.3,h5.4)

alpha(digital)#0.87

##build a CFA measurement model
model1=' digital1 =~ h5.1 + h5.2 + h5.3 + h5.4'
fit1=cfa(model1,data=digital)
summary(fit1,fit.measures=T,standardized=TRUE)
parameterEstimates(fit1)
##look at residule matrix
resid(fit1, type="standardized")
##modification indices
summary(fit1,modindices=T,fit.measures=T)
##caculate reliability(CR)
#定義 functions，請完全照抄
CR = function(fit, xlst) {
  p = parameterEstimates(fit, standardized=T)
  a = sum(p$std.all[p$op == "=~" & p$rhs %in% xlst])^2
  b = sum(p$std.all[p$op == "~~" & p$rhs %in% xlst])
  a / (a+b) }
CR(fit1, c("h5.1","h5.2","h5.3","h5.4")) # 0.87
#計算 Validity (AVE)
AVE = function(fit, xlst) {
  p = parameterEstimates(fit, standardized=T)
  sum(p$std.all[p$op == "=~" & p$rhs %in% xlst]^2) / length(xlst) }
AVE(fit1, c("h5.1","h5.2","h5.3","h5.4")) #0.62
############################
##### 相關性分析（870 人）#####
cor2=select(sele_exp,fknews_expose,prevalence,severity,susceptibility,sele_expo,digital)
all_cor=rcorr(as.matrix(cor2))
str(all_cor)
all_cor
######################
##### 階層迴歸分析（870 人）#####
##### 模型 5-1 #####
model5.1=lm(sele_expo~gender+age+edu+salary,sele_exp)
summary(model5.1)
beta5.1=lm.beta(model5.1)
summary(beta5.1)
vif(model5.1)
##### 模型 5-2 #####
model5.2=lm(sele_expo~gender+age+edu+salary+fknews_expose+prevalence+severity
            +susceptibility,sele_exp)
summary(model5.2)
beta5.2=lm.beta(model5.2)
summary(beta5.2)
vif(model5.2)
anova(model5.1,model5.2)
##### 模型 5-3 #####
model5.3=lm(sele_expo~gender+age+edu+salary+fknews_expose+prevalence+severity
            +susceptibility+mod_fk_di+mod_prvlce_di+mod_svrty_di+mod_suseptblty_di,sele_exp)
summary(model5.3)
beta5.3=lm.beta(model5.3)
summary(beta5.3)
vif(model5.3)
anova(model5.2,model5.3)

