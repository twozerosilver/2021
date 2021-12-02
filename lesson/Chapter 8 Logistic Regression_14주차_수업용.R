#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 8 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------


#----Set the working directory------
getwd()
setwd("C:\\Users\\Owner\\Dropbox\\박사과정\\조교\\2019-2\\통계(학부)_플립러닝\\2. 수업설계\\05. 자료 개발\\03. 11-14주차\\06. In-class\\14주차_로지스틱회귀분석")


#----Install Packages-----
install.packages("car")
install.packages("mlogit")



#------And then load these packages, along with the boot package.-----

library(car)
library(mlogit)


#********************* Eel Example ********************

#load data
eelData<-read.delim("eel.dat", header = TRUE)

#look at first 6 cases of data
head(eelData)
levels(eelData$Cured)
levels(eelData$Intervention)
factor(eelData$Cured)
#Alternatively Re-orders the levels of the factyor so that Not Cured and No Treatment are the baseline categories
eelData$Cured<-factor(eelData$Cured, levels = c("Not Cured", "Cured")) ##팩터 순서지정
levels(eelData$Cured)
eelData$Intervention<-factor(eelData$Intervention, levels = c("No Treatment", "Intervention"))
levels(eelData$Intervention)

# specify the baseline category
eelData$Cured<-relevel(eelData$Cured, "Not Cured") ##기준치 설정
eelData$Intervention<-relevel(eelData$Intervention, "No Treatment")
eelData$Cured<-relevel(eelData$Cured, "Cured") ##기준치 설정
levels(eelData$Cured)
eelData$cure<-ifelse(eelData$Cured=="Cured",1,0)
eelData$Int<-ifelse(eelData$Intervention=="Intervention",1,0)
#Create the two hierarchical models:
a<-glm(cure~Int,data=eelData,family = binomial())
summary(a)
eelModel.1 <- glm(Cured ~ Intervention, data = eelData, family = binomial()) ##binomial
eelModel.2 <- glm(Cured ~ Intervention + Duration, data = eelData, family = binomial())

summary(eelModel.1) ##약을 주면 치료가 안 된 사람의 승산비보다 된 사람의 승산비가 e의 1.2287승배 높다
summary(eelModel.2) ##여기까지랑 exp 정도만 보면 됨

exp(1.2287)
#Just to prove what the null deviance is
eelModel.0 <- glm(Cured ~ 1, data = eelData, family = binomial())
summary(eelModel.0)


modelChi <- eelModel.1$null.deviance - eelModel.1$deviance
chidf <- eelModel.1$df.null - eelModel.1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob



#Compute odds ratio
exp(eelModel.1$coefficients)

#Compute confidence intervals
exp(confint(eelModel.1))
confint(eelModel.1)
#compare model1 and model 2
modelChi <- eelModel.1$deviance - eelModel.2$deviance
chidf <- eelModel.1$df.residual - eelModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(eelModel.1, eelModel.2)


#Diagnostics for model 1

eelData$predicted.probabilities<-fitted(eelModel.1)
eelData$standardized.residuals<-rstandard(eelModel.1)
eelData$studentized.residuals<-rstudent(eelModel.1)
eelData$dfbeta<-dfbeta(eelModel.1)
eelData$dffit<-dffits(eelModel.1)
eelData $leverage<-hatvalues(eelModel.1)

head(eelData[, c("Cured", "Intervention", "Duration", "predicted.probabilities")])
eelData[, c("leverage", "studentized.residuals", "dfbeta")]



#----- Testing multicollinearity ------

vif(eelModel.2) 
1/vif(eelModel.2)