setwd("F:\\研究生课程\\东证期货杯\\选题2竞赛数据-20171114(1)\\竞赛数据-20171114")
library(plyr)
library(dplyr)
library(data.table)
contest_ext_crd_is_ovdsummary<-fread("contest_ext_crd_is_ovdsummary.csv",encoding="UTF-8")
apply(contest_ext_crd_is_ovdsummary,2,function(x) sum(is.na(x)))
apply(contest_ext_crd_is_ovdsummary,2,function(x) sum(x==""))
##type_dw,计算每种贷款数目（即是否有该类贷款）
table(contest_ext_crd_is_ovdsummary$TYPE_DW)
ovdsummary_DW_n<-contest_ext_crd_is_ovdsummary[, .(.N), by = .(REPORT_ID,TYPE_DW)]
#将ovdsummary_DW_n变为宽表
ovdsummary_DW_n1<-dcast(ovdsummary_DW_n, REPORT_ID ~ TYPE_DW, value.var = 'N')
ovdsummary_DW_n1[is.na(ovdsummary_DW_n1)]<-0
ovdsummary_DW_n1
##count_dw,计算每种贷款的贷款逾期笔数
ovdsummary_cdw_n1<-contest_ext_crd_is_ovdsummary[,c(1,2,3)]
sharedebt_cdw_n2<-dcast(ovdsummary_cdw_n1, REPORT_ID ~ TYPE_DW, value.var = 'COUNT_DW')
ovdsummary_cdw_n2
##months,计算每种贷款的贷款逾期月份数
ovdsummary_m_n1<-contest_ext_crd_is_ovdsummary[,c(1,2,4)]
ovdsummary_m_n2<-dcast(ovdsummary_m_n1, REPORT_ID ~ TYPE_DW, value.var = 'MONTHS')
ovdsummary_m_n2
##highest_oa_per_mon,计算每种贷款的贷款单月最高逾期总额
ovdsummary_hom_n1<-contest_ext_crd_is_ovdsummary[,c(1,2,5)]
ovdsummary_hom_n2<-dcast(ovdsummary_hom_n1, REPORT_ID ~ TYPE_DW, value.var = 'HIGHEST_OA_PER_MON')
ovdsummary_hom_n2
##max_duration,计算每种贷款的最大贷款时长
ovdsummary_md_n1<-contest_ext_crd_is_ovdsummary[,c(1,2,6)]
ovdsummary_md_n2<-dcast(ovdsummary_md_n1, REPORT_ID ~ TYPE_DW, value.var = 'MAX_DURATION')
ovdsummary_md_n2









sum(table(contest_ext_crd_is_ovdsummary$HIGHEST_OA_PER_MON))
ncol(contest_ext_crd_is_ovdsummary)
nrow(contest_ext_crd_is_ovdsummary)
head(contest_ext_crd_is_ovdsummary)
str(contest_ext_crd_is_ovdsummary)

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
