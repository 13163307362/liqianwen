setwd("D:\\研究生课程\\东证期货杯\\数据&完成代码&论文\\竞赛数据-20171114")
library(plyr)
library(dplyr)
library(data.table)
library(Rmisc)
#---------------------------------------人口特征--------------------------------
contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")
table(contest_basic_tr$AGENT)
###描述性统计
head(contest_basic_train)
str(contest_basic_train)
table(contest_basic_train$SALARY)
table(contest_basic_train$EDU_LEVEL)
table(contest_basic_train[contest_basic_train$Y==1,]$AGENT)
table(contest_basic_train$AGENT)
table(contest_basic_train$IS_LOCAL)
table(contest_basic_train$WORK_PROVINCE)
table(contest_basic_train$EDU_LEVEL)
table(contest_basic_train$MARRY_STATUS)
table(contest_basic_train$SALARY)
table(contest_basic_train$HAS_FUND)
table(contest_basic_train$Y)
str(contest_basic_train)

#提取身份证户籍和性别信息
contest_basic_train<-mutate(contest_basic_train,huji=substr(contest_basic_train$ID_CARD,1,6),sex=substr(contest_basic_train$ID_CARD,17,17))
str(contest_basic_train)
contest_basic_train$sex[contest_basic_train$sex %in% c("1","3","5","7","9")]<-"男"
contest_basic_train$sex[contest_basic_train$sex %in% c("0","2","4","6","8")]<-"女"

#Agent合为APP、wechat、rongzhijia、ELSE、missing
contest_basic_train$AGENT[contest_basic_train$AGENT==""]<-"missing"
contest_basic_train$AGENT[contest_basic_train$AGENT %in% c("bestpay","chinapnr","fenqile","huifusdb", "ifpp","kuaiqian","orgloan","weijinhui")]<-"others"

#EDU_LEVEL合并为硕士及以上（包含博士研究生、硕士及以上、硕士研究生），本科，专科，专科以下（专科及以下、初中、高中），缺失（包含缺失和其他）
contest_basic_train$EDU_LEVEL[contest_basic_train$EDU_LEVEL %in% c("博士研究生","硕士及以上","硕士研究生")]<-"硕士及以上"
contest_basic_train$EDU_LEVEL[contest_basic_train$EDU_LEVEL %in% c("专科","专科及以下","初中","高中")]<-"专科及以下"
contest_basic_train$EDU_LEVEL[contest_basic_train$EDU_LEVEL %in% c("其他","")]<-"未知"

#SALARY缺失率为0.66665，取值为1、2、3、4、5、6、7，将缺失值编码为-1表示缺失
contest_basic_train$SALARY[is.na(contest_basic_train$SALARY)]<- -1
summary(contest_basic_train)
names(contest_basic_train)[13]<-"gender"
names(contest_basic_train)[12]<-"domicile"

basic<-contest_basic_train[,c("REPORT_ID","AGENT","IS_LOCAL","EDU_LEVEL","MARRY_STATUS","SALARY","HAS_FUND","Y","gender","domicile")]
str(basic)
#------------------------------------资产负债状况------------------------------
loans<-fread("contest_ext_crd_cd_ln.tsv",encoding="UTF-8")
str(loans)
table(loans$state)
table(loans$finance_org)
table(loans$type_dw)
table(loans$guarantee_type)
table(loans$class5_state)
table(loans$balance)
table(loans$balance)
#loans$state为正常的用户 投资（经营）性贷款笔数(每个借款人个人经营性贷款、个人助学贷款和农户贷款账户数目统计)；消费性贷款笔数(个人汽车贷款、个人消费贷款和其他贷款账户数目统计)；住房性贷款笔数(个人商用房（包括商住两用）贷款、个人住房贷款和个人住房公积金贷款账户数目统计)；
#各种贷款笔数
loans_cnt1<-loans[loans$state=="正常",c(2,5)]
n_distinct(loans[loans$state=="正常",c(2,5)]$report_id)
report_wd_amount_n<- loans_cnt1[, .(.N), by = .(report_id,type_dw)]
head(report_wd_amount_n1)
report_wd_amount_n1<-dcast(report_wd_amount_n, report_id ~ type_dw, value.var = 'N')
report_wd_amount_n1[is.na(report_wd_amount_n1)]<-0
nrow(report_wd_amount_n1)
names(report_wd_amount_n1)[5]<-"个人商用房贷款"
loans_cnt<-transmute(report_wd_amount_n1,report_id=report_id,loantype1_cnt=个人经营性贷款+农户贷款+个人助学贷款,loantype2_cnt=个人汽车贷款+个人消费贷款+其他贷款,loantype3_cnt=个人商用房贷款+个人住房贷款+个人住房公积金贷款)

#非信用担保贷记卡账户数表示持有人当期未结清的采用非信用担保方式（保证、抵押担保、其他担保、质押（含保证金）担保、组合（不含保证）担保）的贷记卡数。信用担保贷记卡账户数表示持有人当期未结清的采用信用/免担保方式的贷记卡数；
daijika<-fread("contest_ext_crd_cd_lnd.tsv",encoding="UTF-8")
daijika_cnt1<-daijika[daijika$state=="正常",c(1,8)]
n_distinct(daijika[daijika$state=="正常",c(1,8)]$report_id)
report_daijika_n<- daijika_cnt1[, .(.N), by = .(report_id,guarantee_type)]
head(report_daijika_n1)
report_daijika_n1<-dcast(report_daijika_n, report_id ~ guarantee_type, value.var = 'N')
report_daijika_n1[is.na(report_daijika_n1)]<-0
head(report_daijika_n1)
names(report_daijika_n1)[6]<-"组合担保"
names(report_daijika_n1)[3]<-"信用免担保"
names(report_daijika_n1)[7]<-"质押担保"
names(report_daijika_n1)
daijika_cnt<-transmute(report_daijika_n1,report_id=report_id,guarantee1_cnt=保证+其他担保+抵押担保+质押担保,guarantee2_cnt=信用免担保)
n_distinct(daijika_cnt$guarantee1_cnt)
str(daijika_cnt)
str(loans_cnt)
str(daijika_cnt)

#未销户贷记卡或者未结清贷款合同金额
sharedebt<-fread("contest_ext_crd_is_sharedebt.csv",encoding="UTF-8")
str(sharedebt)
sharedebt_cnt1<-sharedebt[,c(1,2,6)]
head(sharedebt_cnt1)
str(sharedebt_cnt1)
sharedebt_cnt1$CREDIT_LIMIT<-as.numeric(sharedebt_cnt1$CREDIT_LIMIT)

sharedebt_amount<- sharedebt_cnt1[, .(sum(CREDIT_LIMIT)), by = .(REPORT_ID)]
head(sharedebt_amount)
names(sharedebt_amount)[2]<-"debt_amount"
names(sharedebt_amount)[1]<-"report_id"
sharedebt_amount$report_id<-as.numeric(sharedebt_amount$report_id)
str(sharedebt_amount)

union1<-left_join(sharedebt_amount,loans_cnt)
loan_var<-left_join(union1,daijika_cnt)
str(loan_var)

#------------------------------------------信用历史-----------------------------------------
recorddtlinfo<-fread("contest_ext_crd_qr_recorddtlinfo.tsv",encoding="UTF-8")[,-c(2,3)]
head(recorddtlinfo)
str(recorddtlinfo)
apply(recorddtlinfo,2,function(x) sum(is.na(x)))
apply(recorddtlinfo,2,function(x) sum(x==""))
table(recorddtlinfo$query_reason)
##各report_id查询次数
query_n<- recorddtlinfo[, .(.N), by = .(report_id,query_reason)]
query_n1<-dcast(query_n, report_id ~ query_reason, value.var = 'N')

head(query_n1)
query_n1[is.na(query_n1),]<-0
head(query_n1)
names(query_n1)[2]<-"query1"
names(query_n1)[3]<-"query2"
names(query_n1)[4]<-"query3"
names(query_n1)
str(query_n1)

#---------------------------------------------信用行为--------------------------------------
#异常贷款账户数和异常贷记卡账户数是每个借款人当前未结清贷款账户和未注销贷记卡账户的异常账户数统计；
str(sharedebt)
table(sharedebt$TYPE_DW)
abnormal_account_n<- sharedebt[, .(.N), by = .(REPORT_ID,TYPE_DW)]
abnormal_account_n1<-dcast(abnormal_account_n, REPORT_ID ~ TYPE_DW, value.var = 'N')

head(abnormal_account_n1)
abnormal_account_n1[is.na(abnormal_account_n1),]<-0
head(abnormal_account_n1)
names(abnormal_account_n1)[2]<-"account1"
names(abnormal_account_n1)[3]<-"account2"
names(abnormal_account_n1)[4]<-"account3"
names(abnormal_account_n1)
names(abnormal_account_n1)[1]<-"report_id"
abnormal_account_n1$report_id<-as.numeric(abnormal_account_n1$report_id)
str(abnormal_account_n1)


#贷款余额汇总
str(sharedebt)
balance<-sharedebt[sharedebt$TYPE_DW=="未结清贷款信息汇总",c(1,9)]
head(balance)
balance$BALANCE<-as.numeric(balance$BALANCE)
balance_sum<- balance[, .(sum(BALANCE)), by = .(REPORT_ID)]
head(balance_sum)
names(balance_sum)[2]<-"balance_sum"
names(balance_sum)[1]<-"report_id"
balance_sum$report_id<-as.numeric(balance_sum$report_id)
str(balance_sum)

#贷款机构数
str(sharedebt)
institutions<-sharedebt[,c(1,4)]
head(institutions)
institutions$FINANCE_ORG_COUNT<-as.numeric(institutions$FINANCE_ORG_COUNT)
#institutions[is.na(institutions$FINANCE_ORG_COUNT),]
institutions_cnt<- institutions[, .(sum(FINANCE_ORG_COUNT)), by = .(REPORT_ID)]
head(institutions_cnt)
names(institutions_cnt)[2]<-"institutions_cnt"
names(institutions_cnt)[1]<-"report_id"
institutions_cnt$report_id<-as.numeric(institutions_cnt$report_id)
str(institutions_cnt)

#最近6个月平均使用额度汇总
str(sharedebt)
used_amount<-sharedebt[,c(1,11)]
used_amount$LATEST_6M_USED_AVG_AMOUNT <-as.numeric(used_amount$LATEST_6M_USED_AVG_AMOUNT)
#used_amount[is.na(used_amount$LATEST_6M_USED_AVG_AMOUNT),]

head(used_amount)
used_amount_sum<- used_amount[, .(sum(LATEST_6M_USED_AVG_AMOUNT)), by = .(REPORT_ID)]
head(used_amount_sum)
names(used_amount_sum)[2]<-"used_amount_sum"
names(used_amount_sum)[1]<-"report_id"
used_amount_sum$report_id<-as.numeric(used_amount_sum$report_id)
str(used_amount_sum)


#逾期笔数汇总
ovdsummary<-fread("contest_ext_crd_is_ovdsummary.csv",encoding="UTF-8")
apply(ovdsummary,2,function(x) sum(is.na(x)))
apply(ovdsummary,2,function(x) sum(x==""))
head(ovdsummary)
str(ovdsummary)
ovdsummary_cdw_n1<-ovdsummary[,c(1,2,3)]
head(ovdsummary_cdw_n1)
ovdsummary_cdw_n1$COUNT_DW<-as.numeric(ovdsummary_cdw_n1$COUNT_DW)
ovdsummary_cdw_n2<-dcast(ovdsummary_cdw_n1, REPORT_ID ~ TYPE_DW, value.var = 'COUNT_DW')
names(ovdsummary_cdw_n2)[2:4]<-c("ovd_yuqi_n1","ovd_yuqi_n2","ovd_yuqi_n3")
names(ovdsummary_cdw_n2)[1]<-"report_id"
ovdsummary_cdw_n2$report_id<-as.numeric(ovdsummary_cdw_n2$report_id)
str(ovdsummary_cdw_n2)

t4_union1<-left_join(abnormal_account_n1,balance_sum)
t4_union2<-left_join(t4_union1,institutions_cnt)
t4_union3<-left_join(t4_union2,used_amount_sum)
t4_union4<-left_join(t4_union3,ovdsummary_cdw_n2)
str(t4_union4)

#-------------------------------数据汇总--------------------------------------
str(basic)
names(basic)[1]<-"report_id"
str(loan_var)
str(query_n1)
str(t4_union4)
data_union1<-left_join(basic,loan_var)
data_union2<-left_join(data_union1,query_n1)
data_union3<-left_join(data_union2,t4_union4)
str(data_union3)
apply(data_union3,2,function(x) sum(is.na(x)))
table(data_union3$ovd_yuqi_n3)

#ovd_yuqi_n1      ovd_yuqi_n2      ovd_yuqi_n3 的缺失用0填充（表示没有逾期）
data_union3[is.na(data_union3$ovd_yuqi_n1),]$ovd_yuqi_n1<-0
data_union3[is.na(data_union3$ovd_yuqi_n2),]$ovd_yuqi_n2<-0
data_union3[is.na(data_union3$ovd_yuqi_n3),]$ovd_yuqi_n3<-0

#loantype1_cnt    loantype2_cnt    loantype3_cnt的缺失用0填充（表示没有相应的贷款）
data_union3[is.na(data_union3$loantype1_cnt),]$loantype1_cnt<-0
data_union3[is.na(data_union3$loantype2_cnt),]$loantype2_cnt<-0
data_union3[is.na(data_union3$loantype3_cnt),]$loantype3_cnt<-0

#query1           query2           query3的缺失用0填充（表示没有相应的查询）
data_union3[is.na(data_union3$query1),]$query1<-0
data_union3[is.na(data_union3$query2),]$query2<-0
data_union3[is.na(data_union3$query3),]$query3<-0

#guarantee1_cnt         guarantee2_cnt         的缺失用0填充（表示没有相应的担保种类）
data_union3[is.na(data_union3$guarantee1_cnt),]$guarantee1_cnt<-0
data_union3[is.na(data_union3$guarantee2_cnt),]$guarantee2_cnt<-0

#account1          account2  account3         的缺失用0填充（表示没有相应的账户异常）
data_union3[is.na(data_union3$account1),]$account1 <-0
data_union3[is.na(data_union3$account2),]$account2 <-0
data_union3[is.na(data_union3$account3),]$account3 <-0

#institutions_cnt  used_amount_sum  的缺失用0填充（表示没有）
data_union3[is.na(data_union3$institutions_cnt),]$institutions_cnt <-0
data_union3[is.na(data_union3$used_amount_sum),]$used_amount_sum <-0
data_union3[is.na(data_union3$balance_sum),]$balance_sum<-0
data_union3[is.na(data_union3$debt_amount),]$debt_amount<-0
apply(data_union3,2,function(x) sum(is.na(x)))
data1<-na.omit(data_union3)
apply(data1,2,function(x) sum(is.na(x)))
summary(data1)
str(data1)
# nrow(data1[data1$loantype1_cnt==0 & data1$loantype2_cnt==0 & data1$loantype3_cnt==0 & data1$guarantee1_cnt==0 & data1$guarantee2_cnt==0 & data1$query1==0 & data1$query2==0 & data1$query3==0 & data1$account1==0 & data1$account2==0  & data1$account3==0 & data1$ovd_yuqi_n1==0 & data1$ovd_yuqi_n2==0  & data1$ovd_yuqi_n3==0 ,])
# nrow(data1[data1$loantype1_cnt==0 & data1$loantype2_cnt==0 & data1$loantype3_cnt==0 & data1$guarantee1_cnt==0 & data1$guarantee2_cnt==0 & data1$query1==0 & data1$query2==0 & data1$query3==0  & data1$ovd_yuqi_n1==0 & data1$ovd_yuqi_n2==0  & data1$ovd_yuqi_n3==0 ,])
nrow(data1)
nrow(data_union3)
table(data_union3$Y)
table(data1$Y)
