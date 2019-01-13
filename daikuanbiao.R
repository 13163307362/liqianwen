setwd("D:\\东证期货杯\\选题2竞赛数据-20171114(1)\\竞赛数据-20171114")
library(plyr)
library(dplyr)
library(data.table)
contest_ext_crd_cd_ln<-fread("contest_ext_crd_cd_ln.tsv",encoding="UTF-8")[,-6]
table(contest_ext_crd_cd_ln$remain_payment_cyc)
contest_ext_crd_cd_ln$payment_cyc[contest_ext_crd_cd_ln$payment_cyc=="NULL"]<-NA
contest_ext_crd_cd_ln$class5_state[contest_ext_crd_cd_ln$class5_state=="NULL"]<-NA
contest_ext_crd_cd_ln$payment_state[contest_ext_crd_cd_ln$payment_state=="NULL"]<-NA
contest_ext_crd_cd_ln$balance[contest_ext_crd_cd_ln$balance=="NULL"]<-NA
contest_ext_crd_cd_ln$remain_payment_cyc[contest_ext_crd_cd_ln$remain_payment_cyc=="NULL"]<-NA
contest_ext_crd_cd_ln$scheduled_payment_amount[contest_ext_crd_cd_ln$scheduled_payment_amount=="NULL"]<-NA
contest_ext_crd_cd_ln$actual_payment_amount[contest_ext_crd_cd_ln$actual_payment_amount=="NULL"]<-NA
contest_ext_crd_cd_ln$curr_overdue_cyc[contest_ext_crd_cd_ln$curr_overdue_cyc=="NULL"]<-NA
contest_ext_crd_cd_ln$curr_overdue_amount[contest_ext_crd_cd_ln$curr_overdue_amount=="NULL"]<-NA
contest_ext_crd_cd_ln$recent_pay_date[contest_ext_crd_cd_ln$recent_pay_date=="NULL"]<-NA
contest_ext_crd_cd_ln$scheduled_payment_date[contest_ext_crd_cd_ln$scheduled_payment_date=="NULL"]<-NA
contest_ext_crd_cd_ln$end_date[contest_ext_crd_cd_ln$end_date=="NULL"]<-NA
head(contest_ext_crd_cd_ln)
str(contest_ext_crd_cd_ln)
apply(contest_ext_crd_cd_ln,2,function(x) sum(is.na(x)))

contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")
contest_basic_test<-fread("contest_basic_test.tsv",encoding="UTF-8")
names(contest_ext_crd_cd_ln)[2]<-"REPORT_ID"
#contest_ext_crd_cd_ln$REPORT_ID<-as.character(contest_ext_crd_cd_ln$REPORT_ID)
contest_ext_crd_cd_ln1<-inner_join(contest_ext_crd_cd_ln,contest_basic_train[,c(1,11)])
contest_ext_crd_cd_ln2<-inner_join(contest_ext_crd_cd_ln,contest_basic_test[,c(1,10)])
str(contest_ext_crd_cd_ln2)
mean(!complete.cases(contest_ext_crd_cd_ln2))#总体缺失占比
#查看缺失值
library(VIM)
library(mice)
str(contest_ext_crd_cd_ln)
aggr(contest_ext_crd_cd_ln,col = c("skyblue", "pink"), prop=T,numbers=F,only.miss=T,bars=T,labels=paste("ln_",c(1:21)),cex.axis=.6, gap=5)
##计算每个report_id的loan_id数
report_loan_id_n<- contest_ext_crd_cd_ln[, .(.N), by = .(report_id)]
report_loan_id_n1<- report_loan_id_n[, .(report_id=report_id,loan_id_n=N)]
##每个report_id的各账户状态数
report_state_n<- contest_ext_crd_cd_ln[, .(.N), by = .(report_id,state)]
#将report_state_n变为宽表
report_state_n<-dcast(report_state_n, report_id ~ state, value.var = 'N')
report_state_n[is.na(report_state_n)]<-0
report_state_n
##每个report_id贷款机构数
#将436个贷款机构为NA的标识为Na，119个--标识为其本身
contest_ext_crd_cd_ln$finance_org[is.na(contest_ext_crd_cd_ln$finance_org)]<-"Na"
table(contest_ext_crd_cd_ln$finance_org)
#计算id机构数
contest_ext_crd_cd_ln_gg<-contest_ext_crd_cd_ln[,c(2,4)]
contest_ext_crd_cd_ln_gg_n<- contest_ext_crd_cd_ln_gg[, .(.N), by = .(report_id,finance_org)]
head(contest_ext_crd_cd_ln_gg_n1)
contest_ext_crd_cd_ln_gg_n1<- contest_ext_crd_cd_ln_gg_n[,-3][, .(.N), by = .(report_id)]
##每个report_id每种贷款的贷款总额、各种贷款笔数
#计算每个report_id每种type_dw的合同总额
contest_ext_crd_cd_ln_dw<-contest_ext_crd_cd_ln[,c(2,5,11)]
report_wd_amount<- contest_ext_crd_cd_ln_dw[, .(sum(credit_limit_amount)), by = .(report_id,type_dw)]
#将report_wd_amount根据type_dw和credit_limit_amount总额v1变为宽表，即得每个report_id每种贷款的贷款总额
report_wd_amount1<-dcast(report_wd_amount, report_id ~ type_dw, value.var = 'V1')
report_wd_amount1[is.na(report_wd_amount1)]<-0
nrow(report_wd_amount1)
#各种贷款笔数
contest_ext_crd_cd_ln_dw_n<-contest_ext_crd_cd_ln[,c(2,5)]
report_wd_amount_n<- contest_ext_crd_cd_ln_dw_n[, .(.N), by = .(report_id,type_dw)]
report_wd_amount_n1<-dcast(report_wd_amount_n, report_id ~ type_dw, value.var = 'N')
report_wd_amount_n1[is.na(report_wd_amount_n1)]<-0
nrow(report_wd_amount_n1)
report_wd_amount_n1[report_wd_amount_n1$`个人商用房（包括商住两用）贷款`==0,]
report_wd_amount_n1[report_wd_amount_n1$个人住房贷款==7,]
table(report_wd_amount_n1$个人住房贷款)
##各担保方式下的贷款笔数
contest_ext_crd_cd_ln_gt<-contest_ext_crd_cd_ln[,c(2,6)]
contest_ext_crd_cd_ln_gt_n<- contest_ext_crd_cd_ln_gt[, .(.N), by = .(report_id,guarantee_type)]
contest_ext_crd_cd_ln_gt_n1<-dcast(contest_ext_crd_cd_ln_gt_n, report_id ~ guarantee_type, value.var = 'N')
contest_ext_crd_cd_ln_gt_n1[is.na(contest_ext_crd_cd_ln_gt_n1)]<-0
nrow(contest_ext_crd_cd_ln_gt_n1)
##各担保方式贷款总额
contest_ext_crd_cd_ln_gt_amount<-contest_ext_crd_cd_ln[,c(2,6,11)]
report_gt_amount<- contest_ext_crd_cd_ln_gt_amount[, .(sum(credit_limit_amount)), by = .(report_id,guarantee_type)]
#将report_gt_amount根据guarantee_type和credit_limit_amount总额v1变为宽表，即得各担保方式贷款总额
report_gt_amount1<-dcast(report_gt_amount, report_id ~ guarantee_type, value.var = 'V1')
report_gt_amount1[is.na(report_gt_amount1)]<-0
nrow(report_gt_amount1)
##各还款频率下贷款笔数
contest_ext_crd_cd_ln_pr<-contest_ext_crd_cd_ln[,c(2,7)]
contest_ext_crd_cd_ln_pr_n<- contest_ext_crd_cd_ln_pr[, .(.N), by = .(report_id,payment_rating)]
contest_ext_crd_cd_ln_pr_n1<-dcast(contest_ext_crd_cd_ln_pr_n, report_id ~ payment_rating, value.var = 'N')
contest_ext_crd_cd_ln_pr_n1[is.na(contest_ext_crd_cd_ln_pr_n1)]<-0
##各还款频率下贷款总额
contest_ext_crd_cd_ln_pr_amount<-contest_ext_crd_cd_ln[,c(2,7,11)]
report_pr_amount<- contest_ext_crd_cd_ln_pr_amount[, .(sum(credit_limit_amount)), by = .(report_id,payment_rating)]
#将report_pr_amount根据payment_rating和credit_limit_amount总额v1变为宽表，即得各还款频率下贷款总额
report_pr_amount1<-dcast(report_pr_amount, report_id ~ payment_rating, value.var = 'V1')
report_gt_amount1[is.na(report_gt_amount1)]<-0
nrow(report_gt_amount1)
##还款期数
contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$payment_cyc=="NULL",]
contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$class5_state=="NULL",]
table(contest_ext_crd_cd_ln$class5_state)
contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$remain_payment_cyc=="NULL",]
table(contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$remain_payment_cyc!="NULL",]$state)
contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$remain_payment_cyc!="NULL",]
contest_ext_crd_cd_ln[contest_ext_crd_cd_ln$remain_payment_cyc=="12",]
table(contest_ext_crd_cd_ln$payment_cyc)
table(contest_ext_crd_cd_ln$remain_payment_cyc)
##payment_state，24月还款状态
library(stringr)
#24月未开户时长，开户时长用24去减，缺失的地方未开户时长为0
a1<-str_count(contest_ext_crd_cd_ln$payment_state,"/")
length(a1)
#N的个数
a2<-str_count(contest_ext_crd_cd_ln$payment_state,"N")
#*号的个数
a3<-str_count(contest_ext_crd_cd_ln$payment_state,"\\*")
#各数字个数
a4<-str_count(contest_ext_crd_cd_ln$payment_state,"1")
table(a4)
head(contest_ext_crd_cd_ln$payment_state,5)
##去除同时缺失的记录
contest_ext_crd_cd_ln_na1<-contest_ext_crd_cd_ln[!is.na(contest_ext_crd_cd_ln$class5_state),]
contest_ext_crd_cd_ln_na2<-contest_ext_crd_cd_ln[is.na(contest_ext_crd_cd_ln$class5_state),]
contest_ext_crd_cd_ln_na2_1<-contest_ext_crd_cd_ln_na2[contest_ext_crd_cd_ln_na2$state=="呆账",]
contest_ext_crd_cd_ln_na1_1<-contest_ext_crd_cd_ln_na1[contest_ext_crd_cd_ln_na1$state=="逾期",]
contest_ext_crd_cd_ln_na1_1<-contest_ext_crd_cd_ln_na1[contest_ext_crd_cd_ln_na1$state=="正常",]
contest_ext_crd_cd_ln_Mna1<-contest_ext_crd_cd_ln[!is.na(contest_ext_crd_cd_ln$remain_payment_cyc),]
head(contest_ext_crd_cd_ln_Mna1)
table(contest_ext_crd_cd_ln_Mna1$state)
contest_ext_crd_cd_ln_na11<-contest_ext_crd_cd_ln[!is.na(contest_ext_crd_cd_ln$class5_state)&is.na(contest_ext_crd_cd_ln$remain_payment_cyc),]
table(contest_ext_crd_cd_ln_na11$class5_state)
head(contest_ext_crd_cd_ln_na11)
length(table(contest_ext_crd_cd_ln_na1$report_id))
apply(contest_ext_crd_cd_ln_na1,2,function(x) sum(is.na(x)))#只剩finance_org209、payment_cyc25382、remain_payment_cyc25382有缺失
contest_ext_crd_cd_ln_na2<-contest_ext_crd_cd_ln[is.na(contest_ext_crd_cd_ln$class5_state),]
table(contest_ext_crd_cd_ln$state)
table(contest_ext_crd_cd_ln_na2$state)
contest_ext_crd_cd_ln_na1$payment_state
#查看remain_payment_cyc缺失与不缺失记录对应账户状态
remain_payment_cyc_na1<-contest_ext_crd_cd_ln[!is.na(contest_ext_crd_cd_ln$remain_payment_cyc),]
table(remain_payment_cyc_na1$state)
remain_payment_cyc_na2<-contest_ext_crd_cd_ln[is.na(contest_ext_crd_cd_ln$remain_payment_cyc),]
table(remain_payment_cyc_na2$state)



##最后四个时间字段
ncol(contest_ext_crd_cd_ln)
time1<-contest_ext_crd_cd_ln[,c(18,19,20,21)]
min(time1$open_date)
apply(time1,2,function(x) sum(x=="NULL"))
head(time1)
################################贷记卡表#########################################
contest_ext_crd_cd_lnd<-fread("contest_ext_crd_cd_lnd.tsv",encoding="UTF-8")
table(contest_ext_crd_cd_lnd$guarantee_type)
head(contest_ext_crd_cd_lnd)
apply(contest_ext_crd_cd_lnd,2,function(x) sum(is.na(x)))
apply(contest_ext_crd_cd_lnd,2,function(x) sum(x=="NULL"))
##将空值都标识为NA
contest_ext_crd_cd_lnd$used_credit_limit_amount[contest_ext_crd_cd_lnd$used_credit_limit_amount=="NULL"]<-NA
contest_ext_crd_cd_lnd$latest6_month_used_avg_amount[contest_ext_crd_cd_lnd$latest6_month_used_avg_amount=="NULL"]<-NA
contest_ext_crd_cd_lnd$used_highest_amount[contest_ext_crd_cd_lnd$used_highest_amount=="NULL"]<-NA
contest_ext_crd_cd_lnd$scheduled_payment_date[contest_ext_crd_cd_lnd$scheduled_payment_date=="NULL"]<-NA
contest_ext_crd_cd_lnd$scheduled_payment_amount[contest_ext_crd_cd_lnd$scheduled_payment_amount=="NULL"]<-NA
contest_ext_crd_cd_lnd$actual_payment_amount[contest_ext_crd_cd_lnd$actual_payment_amount=="NULL"]<-NA
contest_ext_crd_cd_lnd$recent_pay_date[contest_ext_crd_cd_lnd$recent_pay_date=="NULL"]<-NA
contest_ext_crd_cd_lnd$curr_overdue_cyc[contest_ext_crd_cd_lnd$curr_overdue_cyc=="NULL"]<-NA
contest_ext_crd_cd_lnd$curr_overdue_amount[contest_ext_crd_cd_lnd$curr_overdue_amount=="NULL"]<-NA
contest_ext_crd_cd_lnd$payment_state[contest_ext_crd_cd_lnd$payment_state=="NULL"]<-NA
contest_ext_crd_cd_lnd$cardtype[contest_ext_crd_cd_lnd$cardtype=="NULL"]<-NA

contest_basic_train<-fread("contest_basic_train.tsv",encoding="UTF-8")
contest_basic_test<-fread("contest_basic_test.tsv",encoding="UTF-8")
names(contest_ext_crd_cd_lnd)[1]<-"REPORT_ID"
#contest_ext_crd_cd_ln$REPORT_ID<-as.character(contest_ext_crd_cd_ln$REPORT_ID)
contest_ext_crd_cd_lnd1<-inner_join(contest_ext_crd_cd_lnd,contest_basic_train[,c(1,11)])
contest_ext_crd_cd_lnd2<-inner_join(contest_ext_crd_cd_lnd,contest_basic_test[,c(1,10)])
str(contest_ext_crd_cd_ln2)
mean(!complete.cases(contest_ext_crd_cd_lnd2))#总体缺失占比
#查看缺失值
library(VIM)
library(mice)
str(contest_ext_crd_cd_lnd)
aggr(contest_ext_crd_cd_lnd,col = c("skyblue", "pink"), prop=T,numbers=F,only.miss=T,bars=T,labels=paste("lnd_",c(1:20)),cex.axis=.6, gap=5)


##计算每个report_id的loancard_id数
loancard_id_n<- contest_ext_crd_cd_lnd[, .(.N), by = .(report_id)]
loancard_id_n1<- loancard_id_n[, .(report_id=report_id,loancard_id_n=N)]


##每个report_id的各账户状态数
table(contest_ext_crd_cd_lnd$state)
loancard_state_n<- contest_ext_crd_cd_lnd[, .(.N), by = .(report_id,state)]
#将loancard_state_n变为宽表
loancard_state_n1<-dcast(loancard_state_n, report_id ~ state, value.var = 'N')
loancard_state_n1[is.na(loancard_state_n1)]<-0
loancard_state_n1
##finance_org，计算每个report_id贷款机构数
#finance_org中NA改为Na
contest_ext_crd_cd_lnd$finance_org[is.na(contest_ext_crd_cd_lnd$finance_org)]<-"Na"
table(contest_ext_crd_cd_lnd$finance_org)
#计算id机构数
contest_ext_crd_cd_lnd_gg<-contest_ext_crd_cd_lnd[,c(1,4)]
contest_ext_crd_cd_lnd_gg_n<- contest_ext_crd_cd_lnd_gg[, .(.N), by = .(report_id,finance_org)]
head(contest_ext_crd_cd_lnd_gg_n1)
contest_ext_crd_cd_lnd_gg_n1<- contest_ext_crd_cd_lnd_gg_n[,-3][, .(.N), by = .(report_id)]
##查看成对缺失变量对应账户状态
contest_ext_crd_cd_lnd
#contest_ext_crd_cd_lnd_na1<-contest_ext_crd_cd_lnd[!is.na(contest_ext_crd_cd_lnd$used_credit_limit_amount),]
contest_ext_crd_cd_lnd_na1<-contest_ext_crd_cd_lnd[!is.na(contest_ext_crd_cd_lnd$used_credit_limit_amount),]
contest_ext_crd_cd_lnd_na2<-contest_ext_crd_cd_lnd[is.na(contest_ext_crd_cd_lnd$used_credit_limit_amount),]
length(table(contest_ext_crd_cd_lnd_na1$report_id))
apply(contest_ext_crd_cd_lnd_na1,2,function(x) sum(is.na(x)))#再无缺失
table(contest_ext_crd_cd_lnd$state)
table(contest_ext_crd_cd_lnd_na1$state)
table(contest_ext_crd_cd_lnd_na2$state)















