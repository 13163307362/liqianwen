setwd("F:\\研究生课程\\东证期货杯\\选题2竞赛数据-20171114(1)\\竞赛数据-20171114")
library(plyr)
library(dplyr)
library(data.table)
contest_ext_crd_qr_recordsmr<-fread("contest_ext_crd_qr_recordsmr.tsv",encoding="UTF-8")
apply(contest_ext_crd_qr_recordsmr,2,function(x) sum(is.na(x)))
apply(contest_ext_crd_qr_recordsmr,2,function(x) sum(x==""))
table(contest_ext_crd_qr_recordsmr$type_id)
head(contest_ext_crd_qr_recordsmr)
str(contest_ext_crd_qr_recordsmr)
#############################contest_ext_crd_qr_recorddtlinfo表##############################################################################
recorddtlinfo<-fread("contest_ext_crd_qr_recorddtlinfo.tsv",encoding="UTF-8")[,-3]
head(recorddtlinfo)
str(recorddtlinfo)
apply(recorddtlinfo,2,function(x) sum(is.na(x)))
apply(recorddtlinfo,2,function(x) sum(x==""))
table(recorddtlinfo$query_reason)
##各report_id查询次数
recorddtlinfo_id_n<- recorddtlinfo[, .(.N), by = .(report_id)]
head(recorddtlinfo_id_n)
nrow(recorddtlinfo_id_n)
##query_reason,各原因的查询个数
recorddtlinfo_qr_n<- recorddtlinfo[, .(.N), by = .(report_id,query_reason)]
recorddtlinfo_qr_n1<-dcast(recorddtlinfo_qr_n, report_id ~ query_reason, value.var = 'N')
recorddtlinfo_qr_n1[is.na(recorddtlinfo_qr_n1)]<-0
nrow(recorddtlinfo_qr_n1)
#############################contest_ext_crd_cd_ln_spl表#################################################################################
spl<-fread("contest_ext_crd_cd_ln_spl.tsv",encoding="UTF-8")
head(spl)
str(spl)
apply(spl,2,function(x) sum(is.na(x)))
apply(spl,2,function(x) sum(x=="--"))
length(table(spl$content))
##计算各类贷款的交易数
spl_dw_n<- spl[, .(.N), by = .(report_id,type_dw)]
spl_dw_n1<-dcast(spl_dw_n, report_id ~ type_dw, value.var = 'N')
spl_dw_n1[is.na(spl_dw_n1)]<-0
nrow(spl_dw_n1)
##计算各类贷款的变更月数
spl_cm<- spl[, .(sum(changing_months)), by = .(report_id,type_dw)]
spl_cm1<-dcast(spl_cm, report_id ~ type_dw, value.var = 'V1')
spl_cm1[is.na(spl_cm1)]<-0
nrow(spl_cm1)
##计算各类贷款的发生金额
spl_ca<- spl[, .(sum(changing_amount)), by = .(report_id,type_dw)]
spl_ca1<-dcast(spl_ca, report_id ~ type_dw, value.var = 'V1')
spl_ca1[is.na(spl_ca1)]<-0
nrow(spl_ca1)
#############################contest_ext_crd_cd_lnd_ovd表#################################################################################
contest_ext_crd_cd_lnd_ovd<-fread("contest_ext_crd_cd_lnd_ovd.csv",encoding="UTF-8")
contest_ext_crd_cd_lnd_ovd$AMOUNT[contest_ext_crd_cd_lnd_ovd$AMOUNT==""]<-NA
contest_ext_crd_cd_lnd_ovd$LAST_MONTHS[contest_ext_crd_cd_lnd_ovd$LAST_MONTHS==""]<-NA
nrow(contest_ext_crd_cd_lnd_ovd[contest_ext_crd_cd_lnd_ovd$MONTH_DW=="--            ",])
contest_ext_crd_cd_lnd_ovd$MONTH_DW[contest_ext_crd_cd_lnd_ovd$MONTH_DW=="--            "]<-NA
na1<-contest_ext_crd_cd_lnd_ovd[!is.na(contest_ext_crd_cd_lnd_ovd$MONTH_DW),]

contest_ext_crd_cd_lnd_ovd$MONTH_DW[4]
class(contest_ext_crd_cd_lnd_ovd$MONTH_DW)
apply(contest_ext_crd_cd_lnd_ovd,2,function(x) sum(is.na(x)))
ncol(contest_ext_crd_cd_lnd_ovd)
nrow(contest_ext_crd_cd_lnd_ovd)
head(contest_ext_crd_cd_lnd_ovd)
str(contest_ext_crd_cd_lnd_ovd)




