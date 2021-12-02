
install.packages("psy")
install.packages("psych")
install.packages("pastecs")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corpcor")
install.packages("GPArotation")
install.packages("car")
install.packages("QuantPsyc")

library(QuantPsyc)
library(car)
library(boot)
library(readxl)
library(psych)
library(psy)
library(pastecs)
library(dplyr)
library(readstata13)
library(Hmisc)
library(ggplot2)
library(corrplot)
library(tidyr)
library(corpcor)
library(GPArotation)

HCCP_work <- read.delim("panel.txt", header = TRUE)
getwd()
setwd("C:/Users/parad/Desktop/project/2021")
HCCP_work

head(HCCP_work)
tail(HCCP_work)
dim(HCCP_work)
str(HCCP_work)
summary(HCCP_work)

HCCP_work$postion <- factor(HCCP_work$postion)
HCCP_work$regular <- factor(HCCP_work$regular)
HCCP_work$labor <- factor(HCCP_work$labor)
HCCP_work$sex <- factor(HCCP_work$sex)
HCCP_work$industry <- factor(HCCP_work$industry)
HCCP_work$birthyear <- as.numeric(HCCP_work$birthyear)
HCCP_work$enteryear <- as.numeric(HCCP_work$enteryear)
HCCP_work$create_01 <- as.numeric(HCCP_work$create_01)
HCCP_work$create_02 <- as.numeric(HCCP_work$create_03)
HCCP_work$create_03 <- as.numeric(HCCP_work$create_03)
HCCP_work$collabo_01 <- as.numeric(HCCP_work$collabo_01)
HCCP_work$collabo_02 <- as.numeric(HCCP_work$collabo_02)
HCCP_work$collabo_03 <- as.numeric(HCCP_work$collabo_03)
HCCP_work$control_01 <- as.numeric(HCCP_work$control_01)
HCCP_work$control_02 <- as.numeric(HCCP_work$control_02)
HCCP_work$control_03 <- as.numeric(HCCP_work$control_03)
HCCP_work$compete_01 <- as.numeric(HCCP_work$compete_01)
HCCP_work$compete_02 <- as.numeric(HCCP_work$compete_02)
HCCP_work$compete_03 <- as.numeric(HCCP_work$compete_03)
HCCP_work$commit_01. <- as.numeric(HCCP_work$commit_01.)
HCCP_work$commit_02 <- as.numeric(HCCP_work$commit_02)
HCCP_work$commit_03 <- as.numeric(HCCP_work$commit_03)
HCCP_work$commit_04 <- as.numeric(HCCP_work$commit_04)

describe(HCCP_work)
round(stat.desc(HCCP_work), digits=3)
summary1 <- data.frame(round(stat.desc(HCCP_work), digits=3))
summary2 <- data.frame(round(describe(HCCP_work), digits=3))

library(psych)
## Cronbach's alpha

HCCP_work
data1 <- HCCP_work[, c(4, 5, 7:10, 16:27, 29:32)]
data2 <- HCCP_work[, c(16:27, 29:32)]
data3 <- HCCP_work[, c(16:27, 29:32)]
data4 <- HCCP_work[, c(19:21, 29:32)]
data3$create <- round((data2$create_01+data2$create_02+data2$create_03)/3, digits = 3)
data3$collabo <- round((data2$collabo_01+data2$collabo_02+data2$collabo_03)/3, digits = 3)
data3$control <- round((data2$control_01+data2$control_02+data2$control_03)/3, digits = 3)
data3$compete <- round((data2$compete_01+data2$compete_02+data2$compete_03)/3, digits = 3)
data3$commit <- round((data2$commit_01+data2$commit_02+data2$commit_03)/3, digits = 3)

data_create <- HCCP_work[, c(16, 17, 18)]
data_collabo <- HCCP_work[, c(19, 20, 21)]
data_control <- HCCP_work[, c(22, 23, 24)]
data_compete <- HCCP_work[, c(25, 26, 27)]
data_commit <- HCCP_work[, c(29, 30, 31, 32)]

psych::alpha(data_create)
psych::alpha(data_collabo)
psych::alpha(data_control)
psych::alpha(data_compete)
psych::alpha(data_commit)


##요인분석
pairs(data3[,17:21])

factorMatrix<-round(cor(data5), digits=3)
factorMatrix
write.csv(factorMatrix, file = "FM5.csv")
cortest.bartlett(factorMatrix)
KMO(cor(data4))
det(cor(data4))

data5 <- HCCP_work[, c(19:21, 30:32)]
data6 <- HCCP_work[, c(19:21, 30:32)]
data6$collabo <- round((data2$collabo_01+data2$collabo_02+data2$collabo_03)/3, digits = 3)
data6$commit <- round((data2$commit_01+data2$commit_02+data2$commit_03)/3, digits = 3)
data6$industry <- HCCP_work$industry
data6$enteryear <- HCCP_work$enteryear
data6$position <- HCCP_work$postion
data6$regular <- HCCP_work$regular
data6$labor <- HCCP_work$labor
data6$sex <- HCCP_work$sex
data6

## 더미변수로 바꾸기
levels(data6$sex)
data6$dindustry <- transform(data6, dindustry = ifelse(industry=="manufact", 1, 0),
          dindustry = ifelse(industry=="finance", 1, 0),
          dindustry = ifelse(industry=="nonfinance", 1, 0))
data6$dposition <- transform(data6, dposition = ifelse(position=="a", 1, 0),
          dposition = ifelse(position=="b", 1, 0),
          dposition = ifelse(position=="c", 1, 0),
          dposition = ifelse(position=="d", 1, 0),
          dposition = ifelse(position=="e", 1, 0),
          dposition = ifelse(position=="f", 1, 0),
          dposition = ifelse(position=="g", 1, 0),
          dposition = ifelse(position=="h", 1, 0))
data6$dregular <- transform(data6, dregular = ifelse(regular=="nonregular", 1, 0),
          dregular = ifelse(regular=="regular", 1, 0))
data6$dlabor <- transform(data6, dlabor = ifelse(labor=="nonunion", 1, 0),
          dlabor = ifelse(regular=="union", 1, 0))
data6$dsex <- transform(data6, dsex = ifelse(industry=="female", 1, 0),
          dsex = ifelse(industry=="male", 1, 0))

str(data5)
pc1 <-  principal(data2, nfactors = length(data5), rotate = "none")
pc1
plot(pc1$values, type = "b")

pc2 <-  principal(data2, nfactors = 2, rotate = "none")
pc2
plot(pc2$values, type = "b")

##상관분석

cor0 <- cor(data5, method = "pearson")
cor <- data.frame(round(cor(data3[,17:21]), digits=3))
write.csv(cor0, file = "cor.csv")
plot(cor0)

corr.test(data5,
          use = 'complete',
          method = 'pearson',
          adjust = 'none')

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(cor, histogram=, pch="+")


install.packages("corrplot")
library(corrplot)
aq.cor <- cor(data5, method = "pearson")
aq.cor
corrplot(aq.cor, method="number")


##회귀분석
lmmodel1 <- lm(commitment ~ create + collabo + control + compete, data = HCCP_work)
summary(lmmodel1)

str(data6)
lmmodel2 <- lm(commit ~ collabo+dindustry+enteryear+dposition+dregular+dlabor+dsex, data = data6)
summary(lmmodel2)


##############################################################################

pc_create <- prcomp(data_create)
pc_collabo <- prcomp(data_collabo)
pc_control <- prcomp(data_control)
pc_compete <- prcomp(data_compete)
pc_commit <- prcomp(data_commit)

summary(pc_create)
summary(pc_collabo)
summary(pc_control)
summary(pc_compete)
summary(pc_commit)

plot(pc_create)


en <- eigen(cor(data_create))
names(en)
en$values
plot(en$values, typ="o")

cor(data_create)
cor(data_collabo)
cor(data_control)
cor(data_compete)
cor(data_commit)

##기술통계
install.packages("stargazer")
library(stargazer)
stargazer(data.frame(HCCP_work), type="text", out="기술통계.html")

##상관분석
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(HCCP_work[,11:15])

##상관계수 확인
print(cor <- round(cor(HCCP_work[,11:15])
write.csv(cor,"cor.csv"), row.names=F)

##요인?
str(data3)
pcmodel <-  principal(data3, nfactors = 21, rotate = "none")
pcmodel <-  principal(data2, nfactors = length(data2), rotate = "none")
pc1

##원하는 값(아이겐값, 커뮤넬리티 등)을 따로 보기
#커뮤넬리티
pc1$communality
pc3$communality

print(pc1$communality, cutoff=0, digits=3)
print(pc3$communality, cutoff=0, digits=3)
print(pc4$communality, cutoff=0, digits=3)


#아이겐값값

pc3$values
print(pc3$values, cutoff=0, digits=3)


#Percentage of Variance Accounted For
100*pc3$values[1:23]/length(pc3$values)

print(pc3$loadings, cutoff=0, digits=3, sort = TRUE)
print(pc3$Structure, cutoff=0, digits=3)

print(pc4$loadings, cutoff=0, digits=3, sort = TRUE)
print(pc4$Structure, cut=0.3, digits=3, sort = TRUE)


