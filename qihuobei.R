setwd("D:\\涓滆瘉鏈熻揣鏉痋\閫夐2绔炶禌鏁版嵁-20171114(1)\\绔炶禌鏁版嵁-20171114")
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")
str(contest_basic_train)
bar_AGENT <- ggplot(contest_basic_train, aes(x=Y,fill = as.factor(AGENT))) +
     geom_bar(position = 'fill') + 
     theme_bw() + 
     labs(x = 'Y', y = 'AGENT')
 
bar_AGENT
contest_basic_test<-fread("contest_basic_test.tsv",encoding="UTF-8")
contest_basic<-rbind(contest_basic_train[,-11],contest_basic_test)
apply(contest_basic,2,function(x) sum(is.na(x)))
apply(contest_basic,2,function(x) sum(x==""))
class(contest_basic$EDU_LEVEL)
contest_basic$AGENT[contest_basic$AGENT==""]<-NA
contest_basic$IS_LOCAL[contest_basic$IS_LOCAL==""]<-NA
contest_basic$EDU_LEVEL[contest_basic$EDU_LEVEL==""]<-NA
apply(contest_basic,2,function(x) sum(is.na(x)))
x<-contest_basic$REPORT_ID
y<-apply(contest_basic,1,function(x) sum(is.na(x)))
plot(x,y)
# contest_basic_train$AGENT<-as.factor(contest_basic_train$AGENT)
# contest_basic_train$IS_LOCAL<-as.factor(contest_basic_train$IS_LOCAL)
# contest_basic_train$WORK_PROVINCE<-as.factor(contest_basic_train$WORK_PROVINCE)
# contest_basic_train$EDU_LEVEL<-as.factor(contest_basic_train$EDU_LEVEL)
# contest_basic_train$MARRY_STATUS<-as.factor(contest_basic_train$MARRY_STATUS)
# contest_basic_train$HAS_FUND<-as.factor(contest_basic_train$HAS_FUND)
nrow(contest_basic[complete.cases(contest_basic),])#鏃犵己澶辫褰曟暟
mean(is.na(contest_basic$AGENT))#AGENT缂哄け鍊煎崰姣?
mean(is.na(contest_basic$IS_LOCAL))#IS_LOCAL缂哄け鍊煎崰姣?
mean(is.na(contest_basic$WORK_PROVINCE))#WORK_PROVINCE缂哄け鍊煎崰姣?
mean(is.na(contest_basic$EDU_LEVEL))#EDU_LEVEL缂哄け鍊煎崰姣?
mean(is.na(contest_basic$SALARY))#SALARY缂哄け鍊煎崰姣?
mean(is.na(contest_basic$HAS_FUND))#HAS_FUND缂哄け鍊煎崰姣?
mean(!complete.cases(contest_basic))#鎬讳綋缂哄け鍗犳瘮
#鏌ョ湅鍙橀噺缂哄け涔嬮棿鐨勫叧绯?
# contest_basic_train11<-contest_basic_train[,-c("REPORT_ID","ID_CARD","LOAN_DATE","Y")]
# x<-as.data.frame(abs(is.na(contest_basic_train11)))
# y<-x[which(apply(x,2,sum)>0)]#鎻愬彇鍚己澶卞€肩殑鍙橀噺
# cor(y)
#鏌ョ湅缂哄け鍊?
mean(!complete.cases(contest_basic_test))#鎬讳綋缂哄け鍗犳瘮
library(VIM)
library(mice)
md.pattern(contest_basic_train)
aggr(contest_basic,col = c("skyblue", "pink"), prop=T,numbers=F,only.miss=T,bars=T,labels=names(contest_basic_train),cex.axis=.53, gap=3)

# ##rpart鎻掕ˉ
# library(rpart)
# #缂鸿ˉSALARY
# contest_basic_train1<-contest_basic_train[,-c("REPORT_ID","ID_CARD","LOAN_DATE","AGENT","Y")]
# anova_mod_SALARY<- rpart(SALARY ~ ., data=contest_basic_train1[!is.na(contest_basic_train1$SALARY), ], method="anova", na.action=na.omit)  # ptratio鏄暟鍊煎瀷鍙橀噺
# SALARY_pred <- predict(anova_mod_SALARY, contest_basic_train1[is.na(contest_basic_train1$SALARY), ])
# table(SALARY_pred)
# contest_basic_train$SALARY[is.na(contest_basic_train1$SALARY)]<-SALARY_pred
# #缂鸿ˉAGENT
# class_mod_AGENT<-rpart(AGENT~., data=contest_basic_train1[!is.na(contest_basic_train$AGENT),], na.action=na.omit, method="class")
# AGENT_pred <- predict(class_mod1, contest_basic_train1[is.na(contest_basic_train$AGENT), ])
# predicteds <- as.factor(colnames(AGENT_pred)[apply(AGENT_pred, 1, which.max)])
# ##mice澶氶噸鎻掕ˉ
# library(lattice) #璋冨叆鍑芥暟鍖?  
# library(MASS)  
# library(nnet)
# library(mice)
# memory.limit(size=56000)
# contest_basic_train1<-contest_basic_train[,-c("REPORT_ID","ID_CARD","LOAN_DATE","Y")]
# mice_imp <- mice(contest_basic_train[,c("RAGENT","WORK_PROVINCE","EDU_LEVEL","SALARY","HAS_FUND")], method='rf') 
# # 淇濆瓨瀹屾暣杈撳嚭 
# mice_output <- complete(mice_mod)
#鎻愬彇韬唤璇佹埛绫嶅拰鎬у埆淇℃伅
contest_basic1<-mutate(contest_basic,huji=substr(contest_basic$ID_CARD,1,6),sex=substr(contest_basic$ID_CARD,17,17))
contest_basic1$sex[contest_basic1$sex %in% c("1","3","5","7","9")]<-"鐢?"
contest_basic1$sex[contest_basic1$sex %in% c("0","2","4","6","8")]<-"濂?"
#WORK_PROVINCE銆両S_LOCAL缂哄け鍊肩敤-1琛ラ綈
contest_basic1$WORK_PROVINCE[is.na(contest_basic1$WORK_PROVINCE)]<- -1
contest_basic1$IS_LOCAL[is.na(contest_basic1$IS_LOCAL)]<- "-1"
contest_basic1$WORK_PROVINCE<-as.character(contest_basic1$WORK_PROVINCE)
contest_basic2<-contest_basic1[contest_basic1$IS_LOCAL=="鏈湴绫?" & contest_basic1$WORK_PROVINCE=="-1",]
# contest_basic2$WORK_PROVINCE<-contest_basic2$huji
#鏍规嵁鏈湴绫嶈ˉ榻愰儴鍒哤ORK_PROVINCE缂哄け鍊硷紝鏃犳硶鏍规嵁WORK_PROVINCE琛ラ綈鏄惁鏈湴绫嶏紝鍥犱负鏈湴绫嶇己澶盬ORK_PROVINCE蹇呯己澶?
contest_basic1[contest_basic1$IS_LOCAL=="鏈湴绫?" & contest_basic1$WORK_PROVINCE=="-1",]$WORK_PROVINCE<-contest_basic2$huji
#鏌ョ湅琛ラ綈涔嬪悗缂哄け鎯呭喌
contest_basic1$IS_LOCAL[contest_basic1$IS_LOCAL=="-1"]<-NA
contest_basic1$WORK_PROVINCE[contest_basic1$WORK_PROVINCE=="-1"]<-NA
mean(is.na(contest_basic1$WORK_PROVINCE))#WORK_PROVINCE缂哄け鍊煎崰姣?
apply(contest_basic1,2,function(x) sum(is.na(x)))
#鍒╃敤璁粌鏍锋湰缁樺埗鍚勭渷浠借繚绾︾殑鍦扮悊淇℃伅鍥?
contest_basic_train1<-contest_basic_train[,c("REPORT_ID","Y")]
contest_basic_train1_1<-left_join(contest_basic_train1,contest_basic1)
contest_basic_train1_2<-mutate(contest_basic_train1_1,WORK_PROVINCE_1=substr(contest_basic_train1_1$WORK_PROVINCE,1,2))
contest_basic_train1_2<-contest_basic_train1_2[contest_basic_train1_2$Y==1,c("REPORT_ID","WORK_PROVINCE_1","Y")]
contest_basic_train1_2$WORK_PROVINCE_1[is.na(contest_basic_train1_2$WORK_PROVINCE_1)]<-"-1"
a<-as.data.frame(table(contest_basic_train1_2$WORK_PROVINCE_1))
nrow(a)
#鎴风睄
contest_basic_train1_1_1<-left_join(contest_basic_train1,contest_basic1)
contest_basic_train1_2_1<-mutate(contest_basic_train1_1,huji1=substr(contest_basic_train1_1$ID_CARD,1,2))
contest_basic_train1_2_1<-contest_basic_train1_2_1[contest_basic_train1_2_1$Y==1,c("REPORT_ID","huji1","Y")]
b<-as.data.frame(table(contest_basic_train1_2_1$huji1))
nrow(b)
write.csv(b,"dilixinxi_huji.csv")
data_DFa<-read.csv("鍦扮悊淇℃伅鍥綼.csv")[,-1]
data_DFb<-read.csv("鍦扮悊淇℃伅鍥綽.csv")[,-1]
names(data_DFa)[1]<-"NAME"
#1
library(ggplot2)  
library(plyr)
library(sp)
library(maptools) 
library(RgoogleMaps)
library(ggmap) 
china_map<-readShapePoly("bou2_4p.shp")
x <- china_map@data #鍚勪釜鐪? / 鐩磋緰甯傜殑澶氳竟褰㈤潰鍥?,鍏朵腑 ADCODE99 鏄浗瀹跺熀纭€鍦扮悊淇℃伅涓績瀹氫箟鐨勫尯鍩熶唬鐮?
xs <- data.frame(x,id=seq(0:924)-1)
china_map1 <- fortify(china_map)#鍦板尯缁忕含搴?
china_map_data <- join(china_map1, xs, type = "full") 
table(china_map_data$NAME)
#璇诲叆鎸囨爣鏁版嵁
shujua<-read.csv("D:\\涓滆瘉鏈熻揣鏉痋\閫夐2绔炶禌鏁版嵁-20171114(1)\\绔炶禌鏁版嵁-20171114\\鍦扮悊淇℃伅鍥綼.csv",stringsAsFactors=F)#涓氬姟鏁版嵁
shujub<-read.csv("D:\\涓滆瘉鏈熻揣鏉痋\閫夐2绔炶禌鏁版嵁-20171114(1)\\绔炶禌鏁版嵁-20171114\\鍦扮悊淇℃伅鍥綽.csv",stringsAsFactors=F)#涓氬姟鏁版嵁
china_dataa<-join(china_map_data,shujua,type="full")
china_datab<-join(china_map_data,shujub,type="full")

pa <- ggplot() +
  geom_polygon(data = china_dataa, aes(x=long,y=lat,group=group,fill=Freq),colour="gray40") +
  coord_map() +
  scale_fill_gradient(low="white",high="orange")+
  theme_nothing(legend = TRUE) +
  annotate("text", x=80, y=25, label="鈼忓伐浣滅渷浠借繚绾︽儏鍐?", color= "steelblue", size=3)
pb <- ggplot() +
  geom_polygon(data = china_datab, aes(x=long,y=lat,group=group,fill=Freq),colour="gray40") +
  coord_map() +
  scale_fill_gradient(low="white",high="orange")+
  theme_nothing(legend = TRUE) +
  annotate("text", x=80, y=25, label="鈼忔埛绫嶇渷浠借繚绾︽儏鍐?", color= "steelblue", size=3)

table(china_data$Freq)
library(Rmisc)
multiplot(pa, pb,cols = 2)

china_data[is.na(china_data$Freq),]$Freq<-100

?iconv
#2
library(REmap)
data_DF<-read.csv("鍦扮悊淇℃伅鍥?.csv")[,-1]
result <- remapC(data_DFb,
                 title ="鍚勭渷浠界儹鍔涘浘",
                 maptype = "china",
                 color = "green",
                 theme = get_theme("Bright"),
                 maxdata = 100,
                 mindata = 1)


#鍒╃敤璁粌鏍锋湰鏌ョ湅杩濈害鐨勫鎴锋潵婧愭笭閬撳崰姣?
##鍩虹琛紝杩濈害鐨勫鎴锋潵婧愭笭閬撳崰姣?
table(contest_basic1$EDU_LEVEL)
contest_basic_train2<-contest_basic_train[,c("REPORT_ID","AGENT","Y")][contest_basic_train$Y==1,]
table(contest_basic_train2$AGENT)
contest_basic_train2<-contest_basic_train[,c("REPORT_ID","EDU_LEVEL","Y")][contest_basic_train$Y==1,]
table(contest_basic_train2$EDU_LEVEL)
table(contest_basic$SALARY)

###########################################################################################
contest_ext_crd_hd_report<-fread("contest_ext_crd_hd_report.csv",encoding="UTF-8")
union1<-inner_join(contest_ext_crd_hd_report,contest_basic)
union1_1<-union1[,c("REPORT_ID","REPORT_CREATE_TIME","QUERY_REASON","QUERY_ORG","LOAN_DATE")]
apply(contest_ext_crd_hd_report,2,function(x) sum(is.na(x)))
apply(contest_ext_crd_hd_report,2,function(x) sum(x==""))
ncol(contest_ext_crd_hd_report)
nrow(contest_ext_crd_hd_report)
head(contest_ext_crd_hd_report)
str(contest_ext_crd_hd_report)
table(contest_ext_crd_hd_report$QUERY_ORG)
max(contest_ext_crd_hd_report$REPORT_CREATE_TIME)
min(contest_ext_crd_hd_report$REPORT_CREATE_TIME)
str(contest_ext_crd_hd_report1)
contest_ext_crd_hd_report1<-inner_join(contest_ext_crd_hd_report,contest_basic_train[,c(1,10)])
contest_ext_crd_hd_report1$REPORT_ID<-as.character(contest_ext_crd_hd_report1$REPORT_ID)
nrow(contest_ext_crd_hd_report1)

#################################################################################
contest_ext_crd_cd_ln<-fread("contest_ext_crd_cd_ln.tsv",encoding="UTF-8")
contest_ext_crd_cd_ln$payment_state
table(contest_ext_crd_cd_ln$state)
table(contest_ext_crd_cd_ln$finance_org)
table(contest_ext_crd_cd_ln$type_dw)

contest_ext_crd_cd_ln$payment_cyc<-as.numeric(contest_ext_crd_cd_ln$payment_cyc)
contest_ext_crd_cd_ln$balance<-as.numeric(contest_ext_crd_cd_ln$balance)
contest_ext_crd_cd_ln$remain_payment_cyc<-as.numeric(contest_ext_crd_cd_ln$remain_payment_cyc)
contest_ext_crd_cd_ln$scheduled_payment_amount<-as.numeric(contest_ext_crd_cd_ln$scheduled_payment_amount)
contest_ext_crd_cd_ln$actual_payment_amount<-as.numeric(contest_ext_crd_cd_ln$actual_payment_amount)
contest_ext_crd_cd_ln$curr_overdue_cyc<-as.numeric(contest_ext_crd_cd_ln$curr_overdue_cyc)
contest_ext_crd_cd_ln$curr_overdue_amount<-as.numeric(contest_ext_crd_cd_ln$curr_overdue_amount)
ncol(contest_ext_crd_cd_ln)
nrow(contest_ext_crd_cd_ln)
head(contest_ext_crd_cd_ln)
str(contest_ext_crd_cd_ln)
apply(contest_ext_crd_cd_ln,2,function(x) sum(is.na(x)))
#contest_ext_crd_cd_ln$state[contest_ext_crd_cd_ln$state==""]<-NA
contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$balance=="NULL",]
class(contest_ext_crd_cd_ln$balance)
contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$balance=="NULL",]
summary(contest_ext_crd_cd_ln$credit_limit_amount)
sum(table(contest_ext_crd_cd_ln$credit_limit_amount))
length(table(contest_ext_crd_cd_ln$payment_cyc))

names(contest_ext_crd_cd_ln)[2]<-"REPORT_ID"
contest_ext_crd_cd_ln$REPORT_ID<-as.character(contest_ext_crd_cd_ln$REPORT_ID)

contest_ext_crd_cd_ln1<-inner_join(contest_ext_crd_cd_ln,contest_basic_train[,c(1,10)])
contest_ext_crd_cd_ln2<-inner_join(contest_ext_crd_cd_ln,contest_basic_test[,c(1,10)])
str(contest_ext_crd_cd_ln2)
length(unique(contest_ext_crd_cd_ln1$REPORT_ID))
mean(!complete.cases(contest_ext_crd_cd_ln))#鎬讳綋缂哄け鍗犳瘮
library(VIM)
library(mice)
md.pattern(contest_basic_train)
aggr(contest_basic,col = c("skyblue", "pink"), prop=T,numbers=F,only.miss=T,bars=T,labels=names(contest_basic_train),cex.axis=.53, gap=3)

###################################################################################
contest_ext_crd_cd_lnd<-fread("contest_ext_crd_cd_lnd.tsv",encoding="UTF-8")
contest_ext_crd_cd_lnd$used_credit_limit_amount<-as.numeric(contest_ext_crd_cd_lnd$used_credit_limit_amount)
contest_ext_crd_cd_lnd$latest6_month_used_avg_amount<-as.numeric(contest_ext_crd_cd_lnd$latest6_month_used_avg_amount)
contest_ext_crd_cd_lnd$used_highest_amount<-as.numeric(contest_ext_crd_cd_lnd$used_highest_amount)
contest_ext_crd_cd_lnd$scheduled_payment_amount<-as.numeric(contest_ext_crd_cd_lnd$scheduled_payment_amount)
contest_ext_crd_cd_lnd$actual_payment_amount<-as.numeric(contest_ext_crd_cd_lnd$actual_payment_amount)
contest_ext_crd_cd_lnd$curr_overdue_cyc<-as.numeric(contest_ext_crd_cd_lnd$curr_overdue_cyc)
contest_ext_crd_cd_lnd$curr_overdue_amount<-as.numeric(contest_ext_crd_cd_lnd$curr_overdue_amount)
apply(contest_ext_crd_cd_lnd,2,function(x) sum(is.na(x)))
sum(table(contest_ext_crd_cd_lnd$cardtype))
max(contest_ext_crd_cd_lnd$share_credit_limit_amount)
contest_ext_crd_cd_lnd[contest_ext_crd_cd_lnd$finance_org=="",]
class(contest_ext_crd_cd_lnd$used_credit_limit_amount)
ncol(contest_ext_crd_cd_lnd)
nrow(contest_ext_crd_cd_lnd)
head(contest_ext_crd_cd_lnd)
str(contest_ext_crd_cd_lnd)
names(contest_ext_crd_cd_lnd)[1]<-"REPORT_ID"
contest_ext_crd_cd_lnd$REPORT_ID<-as.character(contest_ext_crd_cd_lnd$REPORT_ID)

contest_ext_crd_cd_lnd1<-inner_join(contest_ext_crd_cd_lnd,contest_basic_train[,c(1,10)])
contest_ext_crd_cd_lnd2<-inner_join(contest_ext_crd_cd_lnd,contest_basic_test[,c(1,10)])

str(contest_ext_crd_cd_lnd2)
length(unique(contest_ext_crd_cd_lnd2$REPORT_ID))
nrow(contest_basic_train)
unique(c(1,1,2))
################################################################################################
contest_ext_crd_is_creditcue<-fread("contest_ext_crd_is_creditcue.csv",encoding="UTF-8")
apply(contest_ext_crd_is_creditcue,2,function(x) sum(is.na(x)))
class(contest_ext_crd_is_creditcue$FIRST_LOAN_OPEN_MONTH)
sum(table(contest_ext_crd_is_creditcue$DISSENT_COUNT))
ncol(contest_ext_crd_is_creditcue)
nrow(contest_ext_crd_is_creditcue)
head(contest_ext_crd_is_creditcue)
str(contest_ext_crd_is_creditcue1)
contest_basic_train$REPORT_ID<-as.character(contest_basic_train$REPORT_ID)
contest_basic_test$REPORT_ID<-as.character(contest_basic_test$REPORT_ID)
contest_ext_crd_is_creditcue1<-inner_join(contest_ext_crd_is_creditcue,contest_basic_train[,c(1,10)])
contest_ext_crd_is_creditcue2<-inner_join(contest_ext_crd_is_creditcue,contest_basic_test[,c(1,10)])

length(unique(contest_ext_crd_is_creditcue2$REPORT_ID))

#############################################################################################
contest_ext_crd_is_sharedebt<-fread("contest_ext_crd_is_sharedebt.csv",encoding="UTF-8")
contest_ext_crd_is_sharedebt$MAX_CREDIT_LIMIT_PER_ORG[contest_ext_crd_is_sharedebt$MAX_CREDIT_LIMIT_PER_ORG==""]<-NA
contest_ext_crd_is_sharedebt$MIN_CREDIT_LIMIT_PER_ORG[contest_ext_crd_is_sharedebt$MIN_CREDIT_LIMIT_PER_ORG==""]<-NA
contest_ext_crd_is_sharedebt$BALANCE[contest_ext_crd_is_sharedebt$BALANCE==""]<-NA
contest_ext_crd_is_sharedebt$USED_CREDIT_LIMIT[contest_ext_crd_is_sharedebt$USED_CREDIT_LIMIT==""]<-NA
max(table(contest_ext_crd_is_sharedebt$CREDIT_LIMIT))
apply(contest_ext_crd_is_sharedebt,2,function(x) sum(is.na(x)))
ncol(contest_ext_crd_is_sharedebt)
nrow(contest_ext_crd_is_sharedebt)
head(contest_ext_crd_is_sharedebt)
str(contest_ext_crd_is_sharedebt)
table(contest_ext_crd_is_sharedebt$TYPE_DW)
contest_basic_train$REPORT_ID<-as.character(contest_basic_train$REPORT_ID)
contest_ext_crd_is_sharedebt1<-inner_join(contest_ext_crd_is_sharedebt,contest_basic_train[,c(1,10)])
contest_basic_test$REPORT_ID<-as.character(contest_basic_test$REPORT_ID)
contest_ext_crd_is_sharedebt2<-inner_join(contest_ext_crd_is_sharedebt,contest_basic_test[,c(1,10)])

length(unique(contest_ext_crd_is_sharedebt2$REPORT_ID))

#########################################################################################
contest_ext_crd_is_ovdsummary<-fread("contest_ext_crd_is_ovdsummary.csv",encoding="UTF-8")
sum(table(contest_ext_crd_is_ovdsummary$HIGHEST_OA_PER_MON))
ncol(contest_ext_crd_is_ovdsummary)
nrow(contest_ext_crd_is_ovdsummary)
head(contest_ext_crd_is_ovdsummary)
str(contest_ext_crd_is_ovdsummary)
contest_basic_train$REPORT_ID<-as.character(contest_basic_train$REPORT_ID)
contest_ext_crd_is_ovdsummary1<-inner_join(contest_ext_crd_is_ovdsummary,contest_basic_train[,c(1,10)])
contest_basic_test$REPORT_ID<-as.character(contest_basic_test$REPORT_ID)
contest_ext_crd_is_ovdsummary2<-inner_join(contest_ext_crd_is_ovdsummary,contest_basic_test[,c(1,10)])

length(unique(contest_ext_crd_is_ovdsummary2$REPORT_ID))

############################################################################################
contest_ext_crd_qr_recordsmr<-fread("contest_ext_crd_qr_recordsmr.tsv",encoding="UTF-8")
sum(table(contest_ext_crd_qr_recordsmr$type_id))
ncol(contest_ext_crd_qr_recordsmr)
nrow(contest_ext_crd_qr_recordsmr)
head(contest_ext_crd_qr_recordsmr)
str(contest_ext_crd_qr_recordsmr)
names(contest_ext_crd_qr_recordsmr)[1]<-"REPORT_ID"
contest_ext_crd_qr_recordsmr$REPORT_ID<-as.character(contest_ext_crd_qr_recordsmr$REPORT_ID)
contest_ext_crd_qr_recordsmr1<-inner_join(contest_ext_crd_qr_recordsmr,contest_basic_train[,c(1,10)])
contest_ext_crd_qr_recordsmr2<-inner_join(contest_ext_crd_qr_recordsmr,contest_basic_test[,c(1,10)])

length(unique(contest_ext_crd_qr_recordsmr2$REPORT_ID))

############################################################################################
contest_ext_crd_qr_recorddtlinfo<-fread("contest_ext_crd_qr_recorddtlinfo.tsv",encoding="UTF-8")
ncol(contest_ext_crd_qr_recorddtlinfo)
nrow(contest_ext_crd_qr_recorddtlinfo)
head(contest_ext_crd_qr_recorddtlinfo)
str(contest_ext_crd_qr_recorddtlinfo)
names(contest_ext_crd_qr_recorddtlinfo)[1]<-"REPORT_ID"
contest_ext_crd_qr_recorddtlinfo$REPORT_ID<-as.character(contest_ext_crd_qr_recorddtlinfo$REPORT_ID)
contest_ext_crd_qr_recorddtlinfo1<-inner_join(contest_ext_crd_qr_recorddtlinfo,contest_basic_train[,c(1,10)])
contest_ext_crd_qr_recorddtlinfo2<-inner_join(contest_ext_crd_qr_recorddtlinfo,contest_basic_test[,c(1,10)])

length(unique(contest_ext_crd_qr_recorddtlinfo1$REPORT_ID))

############################################################################################
contest_ext_crd_cd_ln_spl<-fread("contest_ext_crd_cd_ln_spl.tsv",encoding="UTF-8")
ncol(contest_ext_crd_cd_ln_spl)
nrow(contest_ext_crd_cd_ln_spl)
head(contest_ext_crd_cd_ln_spl)
str(contest_ext_crd_cd_ln_spl)
names(contest_ext_crd_cd_ln_spl)[1]<-"REPORT_ID"
contest_ext_crd_cd_ln_spl$REPORT_ID<-as.character(contest_ext_crd_cd_ln_spl$REPORT_ID)
contest_ext_crd_cd_ln_spl1<-inner_join(contest_ext_crd_cd_ln_spl,contest_basic_train[,c(1,10)])
contest_ext_crd_cd_ln_spl2<-inner_join(contest_ext_crd_cd_ln_spl,contest_basic_test[,c(1,10)])

length(unique(contest_ext_crd_cd_ln_spl1$REPORT_ID))

############################################################################################
contest_ext_crd_cd_lnd_ovd<-fread("contest_ext_crd_cd_lnd_ovd.csv",encoding="UTF-8")
contest_ext_crd_cd_lnd_ovd$AMOUNT[contest_ext_crd_cd_lnd_ovd$AMOUNT==""]<-NA
contest_ext_crd_cd_lnd_ovd$LAST_MONTHS[contest_ext_crd_cd_lnd_ovd$LAST_MONTHS==""]<-NA
nrow(contest_ext_crd_cd_lnd_ovd[contest_ext_crd_cd_lnd_ovd$MONTH_DW=="--            ",])
contest_ext_crd_cd_lnd_ovd$MONTH_DW[4]
class(contest_ext_crd_cd_lnd_ovd$MONTH_DW)
apply(contest_ext_crd_cd_lnd_ovd,2,function(x) sum(is.na(x)))
ncol(contest_ext_crd_cd_lnd_ovd)
nrow(contest_ext_crd_cd_lnd_ovd)
head(contest_ext_crd_cd_lnd_ovd)
str(contest_ext_crd_cd_lnd_ovd)
contest_ext_crd_cd_lnd_ovd1<-inner_join(contest_ext_crd_cd_lnd_ovd,contest_basic_train[,c(1,10)])
contest_ext_crd_cd_lnd_ovd2<-inner_join(contest_ext_crd_cd_lnd_ovd,contest_basic_test[,c(1,10)])

length(unique(contest_ext_crd_cd_lnd_ovd1$REPORT_ID))

###########################################################################################
id2<-union(contest_ext_crd_cd_ln2$REPORT_ID,contest_ext_crd_cd_lnd2$REPORT_ID)
id3<-union(id2,contest_ext_crd_is_creditcue2$REPORT_ID)
id4<-union(id3,contest_ext_crd_is_sharedebt2$REPORT_ID)
id5<-union(id4,contest_ext_crd_qr_recorddtlinfo2$REPORT_ID)
id6<-union(id5,contest_ext_crd_is_ovdsummary2$REPORT_ID)
id7<-union(id6,contest_ext_crd_qr_recordsmr2$REPORT_ID)
id8<-union(id7,contest_ext_crd_cd_ln_spl2$REPORT_ID)
id9<-union(id8,contest_ext_crd_cd_lnd_ovd2$REPORT_ID)

length(id5)
length(unique(contest_ext_crd_qr_recorddtlinfo1$REPORT_ID))

length(union(id9,contest_basic1_train$REPORT_ID))


contest_fraud<-fread("contest_fraud.tsv",encoding="UTF-8")
ncol(contest_fraud)
nrow(contest_fraud)
head(contest_fraud)
str(contest_fraud)