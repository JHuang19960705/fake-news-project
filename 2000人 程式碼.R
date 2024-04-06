# 會用到的套件
packages <- c("psych", "lm.beta", "lavaan", "Hmisc")

# 安裝套件
install.packages(packages[!(packages %in% installed.packages())])

# 載入套件
loaded_packages <- sapply(packages, require, character.only = TRUE)

##### 交乘項計算 #####
#第一步：標準化
names(all)
table(all$fknews_expose)
all$fknews_expose=as.numeric(all$fknews_expose)
all$z_fknews_expose <- scale(all[,6])
all$fknews_expose=as.factor(all$fknews_expose)
all$z_prevalence <- scale(all[,7])
all$z_severity <- scale(all[,8])
all$z_susceptibility <- scale(all[,9])
all$z_digital <- scale(all[,23])
#第二步：相乘
all=all%>%
  mutate(mod_fk_di=(fknews_expose*z_digital))%>%
  mutate(mod_prvlce_di=(z_prevalence*z_digital))%>%
  mutate(mod_svrty_di=(z_severity*z_digital))%>%
  mutate(mod_suseptblty_di=(z_susceptibility*z_digital))
save(all,file="all_1117.Rdata") #存檔
write_sav(all, "all_1117.sav") #存成 SPSS 可讀取之檔案


##### 信效度計算 #####
digiital=select(all,h5.1,h5.2,h5.3,h5.4)
alpha(digital) #數位技巧
med_cre=select(all,i4.1,i4.2,i4.3,i4.4,i4.5)
alpha(med_cre) #新聞媒體可信度
##build a CFA measurement model
model1='med_cre=~ i4.1 + i4.2 + i4.3 + i4.4 + i4.5
 digiital =~ h5.1 + h5.2 + h5.3 + h5.4'
fit1=cfa(model1,data=all)
summary(fit1,fit.measures=T,standardized=TRUE)
parameterEstimates(fit1)
library(gesca)
fit2 <- gesca.run(model1, all,nbt=50)
fitmeasures(fit2)
##look at residule matrix
resid(fit1, type="standardized")
##modification indices
summary(fit1,modindices=T,fit.measures=T)
##caculate reliability(CR)
#定義 functions
CR = function(fit, xlst) {
  p = parameterEstimates(fit, standardized=T)
  a = sum(p$std.all[p$op == "=~" & p$rhs %in% xlst])^2
  b = sum(p$std.all[p$op == "~~" & p$rhs %in% xlst])
  a / (a+b) }
CR(fit1, c("i4.1","i4.2","i4.3","i4.4","i4.5"))
CR(fit1, c("h5.1","h5.2","h5.3","h5.4"))
#計算 Validity (AVE)
AVE = function(fit, xlst) {
  p = parameterEstimates(fit, standardized=T)
  sum(p$std.all[p$op == "=~" & p$rhs %in% xlst]^2) / length(xlst) }
AVE(fit1, c("i4.1","i4.2","i4.3","i4.4","i4.5"))
AVE(fit1, c("h5.1","h5.2","h5.3","h5.4"))
##### 相關性計算 #####
cor=select(all,fknews_expose,prevalence,severity,susceptibility,med_cre,TV_use_log,web_use_log,np_use_log,digital)
all$fknews_expose=as.numeric(all$fknews_expose)
all_cor=rcorr(as.matrix(cor))
str(all_cor)
all_cor
##################
#階層迴歸計算（2000 人）
##### 迴歸 1 可信度 #####
#模型 1-1
model1.1=lm(med_cre~gender+age+edu+salary,all)
summary(model1.1)
beta1.1=lm.beta(model1.1)
summary(beta1.1)
vif(model1.1)
#模型 1-2
model1.2=lm(med_cre~gender+age+edu+salary+fknews_expose+prevalence+severity+susceptibility,all)
summary(model1.2)
beta1.2=lm.beta(model1.2)
summary(beta1.2)
vif(model1.2)
anova(model1.1,model1.2)
##模型 1-3
model1.3=lm(med_cre~gender+age+edu+salary+fknews_expose+prevalence+severity+susceptibility+mod_fk_di+mod_prvlce_di+mod_svrty_di+mod_suseptblty_di,all)
summary(model1.3)
beta1.3=lm.beta(model1.3)
summary(beta1.3)
vif(model1.3)
anova(model1.2,model1.3)
##### 迴歸 2 電視使用 #####
#模型 2-1
model2.1=lm(TV_use_log~gender+age+edu+salary,all)
summary(model2.1)
beta2.1=lm.beta(model2.1)
summary(beta2.1)
vif(model2.1)
#模型 2-2
model2.2=lm(TV_use_log~gender+age+edu+salary+fknews_expose+prevalence+severi
            ty+susceptibility,all)
summary(model2.2)
beta2.2=lm.beta(model2.2)
summary(beta2.2)
anova(model2.1,model2.2)
#模型 2-3
model2.3=lm(TV_use_log~gender+age+edu+salary+fknews_expose+prevalence+severi
            ty+susceptibility+mod_fk_di+mod_prvlce_di+mod_svrty_di+mod_suseptblty_di,all)
summary(model2.3)
beta2.3=lm.beta(model2.3)
summary(beta2.3)
anova(model2.2,model2.3)
vif(model2.3)
#模型 2-4
model2.4=lm(TV_use_log~gender+age+edu+salary+fknews_expose+prevalence+severi
            ty+susceptibility+med_cre,all)
summary(model2.4)
beta2.4=lm.beta(model2.4)
summary(beta2.4)
anova(model2.2,model2.4)

##### 迴歸 3 網路使用 #####
#模型 3-1
model3.1=lm(web_use_log~gender+age+edu+salary,all)
summary(model3.1)
beta3.1=lm.beta(model3.1)
summary(beta3.1)
#模型 3-2
model3.2=lm(web_use_log~gender+age+edu+salary+fknews_expose+prevalence+seve
            rity+susceptibility,all)
summary(model3.2)
beta3.2=lm.beta(model3.2)
summary(beta3.2)
anova(model3.1,model3.2)
#模型 3-3
model3.3=lm(web_use_log~gender+age+edu+salary+fknews_expose+prevalence+seve
            rity+susceptibility+mod_fk_di+mod_prvlce_di+mod_svrty_di+mod_suseptblty_di,all)
summary(model3.3)
beta3.3=lm.beta(model3.3)
summary(beta3.3)
anova(model3.2,model3.3)
#模型 3-4
model3.4=lm(web_use_log~gender+age+edu+salary+fknews_expose+prevalence+seve
            rity+susceptibility+med_cre,all)
summary(model3.4)
beta3.4=lm.beta(model3.4)
summary(beta3.4)
anova(model3.2,model3.4)
vif(model3.4)
##### 迴歸 4 報紙使用 #####
#模型 4-1
model4.1=lm(np_use_log~gender+age+edu+salary,all)
summary(model4.1)
beta4.1=lm.beta(model4.1)
summary(beta4.1)
#模型 4-2
model4.2=lm(np_use_log~gender+age+edu+salary+fknews_expose+prevalence+severit
            y+susceptibility,all)
summary(model4.2)
beta4.2=lm.beta(model4.2)
summary(beta4.2)
anova(model4.1,model4.2)
#模型 4-3
model4.3=lm(np_use_log~gender+age+edu+salary+fknews_expose+prevalence+severit
            y+susceptibility+mod_fk_di+mod_prvlce_di+mod_svrty_di+mod_suseptblty_di,all)
summary(model4.3)
beta4.3=lm.beta(model4.3)
summary(beta4.3)
anova(model4.2,model4.3)
#模型 4-4
model4.4=lm(np_use_log~gender+age+edu+salary+fknews_expose+prevalence+severit
            y+susceptibility+med_cre,all)
summary(model4.4)
beta4.4=lm.beta(model4.4)
summary(beta4.4)
anova(model4.2,model4.4)
vif(model4.4)
#####################
###### RQ2 假新聞的暴露與感知普遍性、嚴重性、易感性是否具有數位落差？（2000 人）######
table(all$digital)
hist(all$digital)
describeBy(all$digital)
summary(all$digital)
all$t_test_digital=cut(all$digital,breaks=2.39,
                       labels=c('low', 'high'))
t.test(digital~fknews_expose, data=all, mu=0, alt="two.sided", conf=0.95, var.eq=T,
       paired=F )
t.test(prevalence~t_test_digital, data=all, mu=0, alt="two.sided", conf=0.95, var.eq=T,
       paired=F )
t.test(severity~t_test_digital, data=all, mu=0, alt="two.sided", conf=0.95, var.eq=T,
       paired=F )
t.test(susceptibility~t_test_digital, data=all, mu=0, alt="two.sided", conf=0.95,
       var.eq=F, paired=F )
