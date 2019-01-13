library(mlbench)
data ("BostonHousing", package="mlbench")
original <- BostonHousing
head(original)
nrow(original)
set.seed(100)
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] 
library(mice)
md.pattern(BostonHousing)
library(rpart)
class_mod <- rpart(rad ~ . - medv, data=BostonHousing[!is.na(BostonHousing$rad), ], method="class", na.action=na.omit)  # rad变量是因子型变量
head(class_mod)
rad_pred <- predict(class_mod, BostonHousing[is.na(BostonHousing$rad), ])
predicteds <- as.numeric(colnames(rad_pred)[apply(rad_pred, 1, which.max)])
predicteds <- colnames(rad_pred)[apply(rad_pred, 1, which.max)]

BostonHousing$rad[60]

library(mice)
miceMod <- mice(BostonHousing[, !names(BostonHousing) %in% "medv"], method="rf")  # 基于随机森林模型进行mice插值
miceOutput <- complete(miceMod)  # 生成完整数据
anyNA(miceOutput)
#查看缺失值的情况
require(magrittr)
require(Amelia)
missmap(BostonHousing, col=c("black", "grey"), legend=FALSE, main = 'Missing Map')
