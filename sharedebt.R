setwd("F:\\研究生课程\\东证期货杯\\选题2竞赛数据-20171114(1)\\竞赛数据-20171114")
library(plyr)
library(dplyr)
library(data.table)
contest_ext_crd_is_sharedebt<-fread("contest_ext_crd_is_sharedebt.csv",encoding="UTF-8")
contest_ext_crd_is_sharedebt$MAX_CREDIT_LIMIT_PER_ORG[contest_ext_crd_is_sharedebt$MAX_CREDIT_LIMIT_PER_ORG==""]<-NA
contest_ext_crd_is_sharedebt$MIN_CREDIT_LIMIT_PER_ORG[contest_ext_crd_is_sharedebt$MIN_CREDIT_LIMIT_PER_ORG==""]<-NA
contest_ext_crd_is_sharedebt$BALANCE[contest_ext_crd_is_sharedebt$BALANCE==""]<-NA
contest_ext_crd_is_sharedebt$USED_CREDIT_LIMIT[contest_ext_crd_is_sharedebt$USED_CREDIT_LIMIT==""]<-NA
apply(contest_ext_crd_is_sharedebt,2,function(x) sum(is.na(x)))
apply(contest_ext_crd_is_sharedebt,2,function(x) sum(x==""))
ncol(contest_ext_crd_is_sharedebt)
nrow(contest_ext_crd_is_sharedebt)
head(contest_ext_crd_is_sharedebt)
str(contest_ext_crd_is_sharedebt)
##type_dw,计算每种贷款数目（即是否有该类贷款）
table(contest_ext_crd_is_sharedebt$TYPE_DW)
sharedebt_DW_n<-contest_ext_crd_is_sharedebt[, .(.N), by = .(REPORT_ID,TYPE_DW)]
#将sharedebt_DW_n变为宽表
sharedebt_DW_n1<-dcast(sharedebt_DW_n, REPORT_ID ~ TYPE_DW, value.var = 'N')
sharedebt_DW_n1[is.na(sharedebt_DW_n1)]<-0
sharedebt_DW_n1
##finance_corp_count，计算各类贷款的贷款法人数
sharedebt_fcc_n1<-contest_ext_crd_is_sharedebt[,c(1,2,3)]
sharedebt_fcc_n2<-dcast(sharedebt_fcc_n1, REPORT_ID ~ TYPE_DW, value.var = 'FINANCE_CORP_COUNT')
sharedebt_fcc_n2[is.na(sharedebt_fcc_n2)]<-0
sharedebt_fcc_n2
##finance_org_count计算各类贷款的贷款机构数
sharedebt_foc_n1<-contest_ext_crd_is_sharedebt[,c(1,2,4)]
sharedebt_foc_n2<-dcast(sharedebt_foc_n1, REPORT_ID ~ TYPE_DW, value.var = 'FINANCE_ORG_COUNT')
sharedebt_foc_n2[is.na(sharedebt_foc_n2)]<-0
sharedebt_foc_n2
##account_count,计算各类贷款的贷款账户数
sharedebt_ac_n1<-contest_ext_crd_is_sharedebt[,c(1,2,5)]
sharedebt_ac_n2<-dcast(sharedebt_ac_n1, REPORT_ID ~ TYPE_DW, value.var = 'ACCOUNT_COUNT')
sharedebt_ac_n2[is.na(sharedebt_ac_n2)]<-0
sharedebt_ac_n2
##credit_limit,计算各类贷款的合同金额
sharedebt_cl_n1<-contest_ext_crd_is_sharedebt[,c(1,2,6)]
sharedebt_cl_n2<-dcast(sharedebt_cl_n1, REPORT_ID ~ TYPE_DW, value.var = 'CREDIT_LIMIT')
sharedebt_cl_n2[is.na(sharedebt_cl_n2)]<-0
sharedebt_cl_n2
##查看成对缺失情况
sharedebt_na1<-contest_ext_crd_is_sharedebt[!is.na(contest_ext_crd_is_sharedebt$MAX_CREDIT_LIMIT_PER_ORG),]
length(table(sharedebt_na1$REPORT_ID))
apply(sharedebt_na1,2,function(x) sum(is.na(x)))#只剩finance_org209、payment_cyc25382、remain_payment_cyc25382有缺失
sharedebt_na2<-contest_ext_crd_is_sharedebt[is.na(contest_ext_crd_is_sharedebt$MAX_CREDIT_LIMIT_PER_ORG),]
table(sharedebt_na1$TYPE_DW)
table(sharedebt_na2$TYPE_DW)
##latest_6m_used_avg_amount,计算各类贷款的最近6个月平均使用额度
sharedebt_l6a_n1<-contest_ext_crd_is_sharedebt[,c(1,2,11)]
sharedebt_l6a_n2<-dcast(sharedebt_l6a_n1, REPORT_ID ~ TYPE_DW, value.var = 'LATEST_6M_USED_AVG_AMOUNT')
sharedebt_l6a_n2[is.na(sharedebt_l6a_n2)]<-0
sharedebt_l6a_n2


