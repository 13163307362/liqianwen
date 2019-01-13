setwd("D:\\东证期货杯\\选题2竞赛数据-20171114(1)\\竞赛数据-20171114")
library(plyr)
library(dplyr)
library(data.table)
library(Rmisc)
contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")
###描述性统计
table(contest_basic_train$SALARY)


contest_basic_test<-fread("contest_basic_test.tsv",encoding="UTF-8")
contest_basic<-rbind(contest_basic_train[,-11],contest_basic_test)
#Agent合为APP、wechat、rongzhijia、ELSE、missing
contest_basic$AGENT[contest_basic$AGENT==""]<-"missing"
contest_basic$AGENT[contest_basic$AGENT %in% c("其他","应用商店","搜索引擎","收到邮件或短信","新闻资讯平台", "朋友推荐","社交软件平台","第三方贷款平台","bestpay","chinapnr","DDH_SYNC","fenqile","huifusdb","ifpp","kuaiqian","orgloan","weijinhui")]<-"ELSE"
#提取身份证户籍和性别信息、补齐部分WORK_PROVINCE
contest_basic1<-mutate(contest_basic,huji=substr(contest_basic$ID_CARD,1,6),sex=substr(contest_basic$ID_CARD,17,17))
contest_basic1$sex[contest_basic1$sex %in% c("1","3","5","7","9")]<-"男"
contest_basic1$sex[contest_basic1$sex %in% c("0","2","4","6","8")]<-"女"
#根据本地籍补齐部分WORK_PROVINCE缺失值，无法根据WORK_PROVINCE补齐是否本地籍，因为本地籍缺失WORK_PROVINCE必缺失
class(contest_basic1$huji)
class(contest_basic1$WORK_PROVINCE)
contest_basic1[contest_basic1$IS_LOCAL=="本地籍" & is.na(contest_basic1$WORK_PROVINCE),]$WORK_PROVINCE<-contest_basic1[contest_basic1$IS_LOCAL=="本地籍" & is.na(contest_basic1$WORK_PROVINCE),]$huji
#work_province合为江苏、河北、山西、辽宁、黑龙江、吉林、其他、缺失
#contest_basic1$WORK_PROVINCE[is.na(contest_basic1$WORK_PROVINCE)]<-"missing"
contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="32"]<-"江苏"
contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="13"]<-"河北"
contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="14"]<-"山西"
contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="21"]<-"辽宁"
contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="23"]<-"黑龙江"
contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2)=="22"]<-"吉林"
quhao<-c("11","12","15","31","33","34","35","36","37","41","42","43","44","45","46","50","51","52","53","54","61","62","63","64","65")
contest_basic1$WORK_PROVINCE[substr(contest_basic1$WORK_PROVINCE,1,2) %in% quhao]<-"其他"
table(contest_basic1$WORK_PROVINCE)
#EDU_LEVEL合并为硕士及以上（包含博士研究生、硕士及以上、硕士研究生），本科，专科，专科以下（专科及以下、初中、高中），缺失（包含缺失和其他）
contest_basic1$EDU_LEVEL[contest_basic1$EDU_LEVEL %in% c("博士研究生","硕士及以上","硕士研究生")]<-"硕士及以上"
contest_basic1$EDU_LEVEL[contest_basic1$EDU_LEVEL %in% c("专科及以下","初中","高中")]<-"专科以下"
contest_basic1$EDU_LEVEL[contest_basic1$EDU_LEVEL %in% c("其他","")]<-"未知"
#SALARY缺失率为0.66665，取值为1、2、3、4、5、6、7，将缺失值编码为-1表示缺失
contest_basic1$SALARY[is.na(contest_basic1$SALARY)]<- -1
# #用mice多重插补
# contest_basic1$MARRY_STATUS[contest_basic1$MARRY_STATUS==""]<-NA
# contest_basic1$IS_LOCAL[contest_basic1$IS_LOCAL==""]<-NA
# contest_basic1$AGENT<-as.factor(contest_basic1$AGENT)
# contest_basic1$IS_LOCAL<-as.factor(contest_basic1$IS_LOCAL)
# contest_basic1$WORK_PROVINCE<-as.factor(contest_basic1$WORK_PROVINCE)
# contest_basic1$EDU_LEVEL<-as.factor(contest_basic1$EDU_LEVEL)
# contest_basic1$MARRY_STATUS<-as.factor(contest_basic1$MARRY_STATUS)
# contest_basic1$SALARY<-as.factor(contest_basic1$SALARY)
# contest_basic1$HAS_FUND<-as.factor(contest_basic1$HAS_FUND)
# contest_basic1$sex<-as.factor(contest_basic1$sex)
# library(mice)  
# miceMod<-mice(contest_basic1[,!names(contest_basic1) %in% c("REPORT_ID","ID_CARD","LOAN_DATE","huji")], method="rf")#基于随机森林模型进行多重插补  
# miceOutput<-complete(miceMod)#生成完整数据 
# sum(is.na(miceOutput))
# str(contest_basic1)
# #用rpart插补缺失值
library(rpart)
contest_basic1$MARRY_STATUS[contest_basic1$MARRY_STATUS==""]<-NA
contest_basic1$IS_LOCAL[contest_basic1$IS_LOCAL==""]<-NA
contest_basic1$AGENT<-as.factor(contest_basic1$AGENT)
contest_basic1$IS_LOCAL<-as.factor(contest_basic1$IS_LOCAL)
contest_basic1$WORK_PROVINCE<-as.factor(contest_basic1$WORK_PROVINCE)
contest_basic1$EDU_LEVEL<-as.factor(contest_basic1$EDU_LEVEL)
contest_basic1$MARRY_STATUS<-as.factor(contest_basic1$MARRY_STATUS)
contest_basic1$SALARY<-as.factor(contest_basic1$SALARY)
contest_basic1$HAS_FUND<-as.factor(contest_basic1$HAS_FUND)
contest_basic1$sex<-as.factor(contest_basic1$sex)
#缺补IS_LOCAL
class_mod_IS_LOCAL<- rpart(IS_LOCAL ~ ., data=contest_basic1[!is.na(contest_basic1$IS_LOCAL),!(names(contest_basic1)  %in% c("REPORT_ID","ID_CARD","LOAN_DATE","huji"))], method="class", na.action=na.omit)  
IS_LOCAL_pred <- predict(class_mod_IS_LOCAL, contest_basic1[is.na(contest_basic1$IS_LOCAL), ])
predicteds_IS_LOCAL <- as.factor(colnames(IS_LOCAL_pred)[apply(IS_LOCAL_pred, 1, which.max)])
contest_basic1$IS_LOCAL[is.na(contest_basic1$IS_LOCAL)]<-predicteds_IS_LOCAL
#缺补WORK_PROVINCE
class_mod_WORK_PROVINCE<-rpart(WORK_PROVINCE~., data=contest_basic1[!is.na(contest_basic1$WORK_PROVINCE),!(names(contest_basic1)  %in% c("REPORT_ID","ID_CARD","LOAN_DATE","huji"))], na.action=na.omit, method="class")
WORK_PROVINCE_pred <- predict(class_mod_WORK_PROVINCE, contest_basic1[is.na(contest_basic1$WORK_PROVINCE), ])
predicteds_WORK_PROVINCE <- as.factor(colnames(WORK_PROVINCE_pred)[apply(WORK_PROVINCE_pred, 1, which.max)])
contest_basic1$WORK_PROVINCE[is.na(contest_basic1$WORK_PROVINCE)]<-predicteds_WORK_PROVINCE
#缺补HAS_FUND
class_mod_HAS_FUND<-rpart(HAS_FUND~., data=contest_basic1[!is.na(contest_basic1$HAS_FUND),!(names(contest_basic1)  %in% c("REPORT_ID","ID_CARD","LOAN_DATE","huji"))], na.action=na.omit, method="class")
HAS_FUND_pred <- predict(class_mod_HAS_FUND, contest_basic1[is.na(contest_basic1$HAS_FUND), ])
predicteds_HAS_FUND <- as.factor(colnames(HAS_FUND_pred)[apply(HAS_FUND_pred, 1, which.max)])
contest_basic1$HAS_FUND[is.na(contest_basic1$HAS_FUND)]<-predicteds_HAS_FUND
#缺补MARRY_STATUS 
class_mod_MARRY_STATUS <-rpart(MARRY_STATUS~., data=contest_basic1[!is.na(contest_basic1$MARRY_STATUS),!(names(contest_basic1)  %in% c("REPORT_ID","ID_CARD","LOAN_DATE","huji"))], na.action=na.omit, method="class")
MARRY_STATUS_pred <- predict(class_mod_MARRY_STATUS, contest_basic1[is.na(contest_basic1$MARRY_STATUS), ])
predicteds_MARRY_STATUS <- as.factor(colnames(MARRY_STATUS_pred)[apply(MARRY_STATUS_pred, 1, which.max)])
contest_basic1$MARRY_STATUS[is.na(contest_basic1$MARRY_STATUS)]<-predicteds_MARRY_STATUS
apply(contest_basic1,2,function(x) sum(is.na(x)))
write.csv(contest_basic1,"jichubiao_feacture.csv")

contest_basic_train$AGENT<-as.factor(contest_basic_train$AGENT)
bar_AGENT <- ggplot(contest_basic_train, aes(x=as.factor(Y),fill = AGENT)) +
  geom_bar(position = 'fill') + 
  theme_bw() + 
  guides(fill=guide_legend(title=NULL))+
  labs(x = 'Y', y = 'AGENT')
bar_AGENT
contest_basic_train1$AGENT<-as.factor(contest_basic_train1$AGENT)
contest_basic_train1$MARRY_STATUS<-as.factor(contest_basic_train1$MARRY_STATUS)
contest_basic_train1$EDU_LEVEL<-as.factor(contest_basic_train1$EDU_LEVEL)
contest_basic_train1$SALARY<-as.factor(contest_basic_train1$SALARY)
contest_basic_train1$AGENT<-as.factor(contest_basic_train1$AGENT)
contest_basic_train1$<-as.factor(contest_basic_train1$AGENT)
head(contest_basic1)
contest_basic_train1<-left_join(contest_basic_train[,c(1,11)],contest_basic1)
contest_basic1$REPORT_ID<-as.character(contest_basic1$REPORT_ID)

bar_EDU_LEVEL<- ggplot(contest_basic_train1, aes(x=as.factor(Y),fill = EDU_LEVEL)) +
  geom_bar(position = 'fill') + 
  theme_bw() + 
  guides(fill=guide_legend(title=NULL))+
  labs(x = 'Y', y = 'EDU_LEVEL')
bar_EDU_LEVEL

bar_MARRY_STATUS<- ggplot(contest_basic_train1, aes(x=as.factor(Y),fill = MARRY_STATUS)) +
  geom_bar(position = 'fill') + 
  theme_bw() + 
  guides(fill=guide_legend(title=NULL))+
  labs(x = 'Y', y = 'MARRY_STATUS')
bar_MARRY_STATUS

bar_SALARY<- ggplot(contest_basic_train1, aes(x=as.factor(Y),fill = SALARY)) +
  geom_bar(position = 'fill') + 
  theme_bw() + 
  guides(fill=guide_legend(title=NULL))+
  labs(x = 'Y', y = 'SALARY')
bar_SALARY 

bar_HAS_FUND <- ggplot(contest_basic_train1[!is.na(contest_basic_train1$HAS_FUND),], aes(x=as.factor(Y),fill = HAS_FUND)) +
  geom_bar(position = 'fill') + 
  theme_bw() + 
  guides(fill=guide_legend(title=NULL))+
  labs(x = 'Y', y = 'HAS_FUND')
bar_IS_LOCAL<- ggplot(contest_basic_train1, aes(x=as.factor(Y),fill = IS_LOCAL)) +
  geom_bar(position = 'fill') + 
  theme_bw() + 
  guides(fill=guide_legend(title=NULL))+
  labs(x = 'Y', y = 'IS_LOCAL')

bar_IS_LOCAL
bar_sex <- ggplot(contest_basic_train1, aes(x=as.factor(Y),fill = sex)) +
  geom_bar(position = 'fill') + 
  theme_bw() +
  guides(fill=guide_legend(title=NULL))+
  labs(x = 'Y', y = 'SEX')
bar_sex
multiplot( bar_AGENT,bar_EDU_LEVEL, bar_MARRY_STATUS, bar_sex ,cols = 2)
multiplot(box_SALARY,bar_HAS_FUND ,cols = 2)

box_SALARY <- ggplot(contest_basic_train1[contest_basic_train1$SALARY!=0,], aes(x = as.factor(Y), y = SALARY, fill = as.factor(Y))) + 
  geom_boxplot() + 
  theme_bw() + 
  guides(fill=guide_legend(title=NULL))+
  labs(x = 'Y', y = 'SALARY')
table(contest_basic_train1$SALARY)
contest_basic_train1$SALARY<-as.numeric(contest_basic_train1$SALARY)
contest_basic_train1$SALARY<-contest_basic_train1$SALARY-1
contest_basic_train1[contest_basic_train1$SALARY==1,]
box_time
table(contest_basic_train1$SALARY)

# 合并这些图形在一个绘图区域，cols = 2的意思就是排版为一行二列
multiplot(box_sat, box_eva, box_mon, box_time, cols = 2)

