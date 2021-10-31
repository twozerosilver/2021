##-------다항 로지스틱 회귀분석 R 실습
##-------앤디 필드의 유쾌한 R 통계학(2019)
##-------'작업 멘트'를 평가하는 방식에 관한 연구에서 mlogit으로 분석하기

##-------패키지 설치하기
install.packages("car")
install.packages("mlogit")

library(car)
library(mlogit)

##-------디렉토리 설정하기
getwd()

##-------데이터 불러오기
chatData<-read.delim("Chat-Up Lines2.dat", header = TRUE)

##-------데이터를 확인하기
##텍스트 문자열이 포함되어 있으므로 요인여부 확인하고, 필요 시 요인으로 변환하기
##해석을 고려하여 기저범주 설정하기: 현재는 Female이 기저범주. 
##여성에 대한 작업멘트의 성공 여부가 궁금하다면 기저범주는 male로 두어야 함

is.na(chatData)
sum(is.na(chatData))

head(chatData, 10)

is.factor(chatData$Success)
is.factor(chatData$Gender)
chatData$Success=factor(chatData$Success)
chatData$Gender=factor(chatData$Gender)

str(chatData)
levels(chatData$Success)
levels(chatData$Gender)
chatData$Gender<-relevel(chatData$Gender,"Male")

chatData$Success<-factor(chatData$Success, levels = c("No response/Walk Off", "Get Phone Number", "Go On A Date"))
levels(chatData$Success)
chatData$Gender<-factor(chatData$Gender, levels = c("Male", "Female"))
levels(chatData$Gender)

##-------기존의 데이터를 mlogit에 적정한 형태로 바꾸기
##newDataframe<-mlogit.data(oldDataFrame, choice = "text", shape = "wide"/"long")
##long layout: 1행 1 관측치, wide layout: 1행 여러 데이터
##각 행이 3가지 종속변수(Success)에 대응하여 분리됨
mlChat <- mlogit.data(chatData, choice="Success", shape="wide")
head(mlChat, 10)

##-------다항 로지스틱 회귀분석 모델 만들기
##newDataframe<-mlogit.data(결과변수~예측변수, data = 데이터프레임, na.action = 결측값 처리방식, relevel = 결과 기저 범주 번호)
##기존 lm이나 glm에 비해 "reflevel"을 해야 한다는 특징
##기저범주 설정하기: No response/Walk off
##예측변수 결정하기: Good_Mate, Funny, Sexual
chatModel <- mlogit(Success ~ 1 | Good_Mate + Funny + Sexual + Gender, data = mlChat, reflevel = "No response/Walk Off")
summary(chatModel)

##-------회귀모형 해석하기
chatBase <- mlogit(Success ~ 1, data = mlChat, reflevel = "No response/Walk Off")
summary(chatBase)

##chatBase의 Log-Likelihood: Log-Likelihood: -1008 / chatModel은 Log-Likelihood: -892.02
1008-892.02
(1008-892.02)*-2
##둘의 차이는 115.98, 카이제곱 -2LL=231.96
##Likelihood ratio test : chisq = 231.95 (p.value = < 2.22e-16)
##전체 모형의 가능도비에 따라 이러한 변화는 유의하다
##chatBase 보다 chatModel이 더 적합하다(많이 설명한다)

##-------전화번호를 얻을 확률은 어떻게 변화할까
exp(chatModel$coefficients)
data.frame(exp(chatModel$coefficients))
exp(confint(chatModel))

** 신뢰구간이 1을 포함한다면 모델은 유의하지 않다

##-------3. Good_Mate:Get Phone Number
##no response와 get phone number를 유의미하게 예측(b=0.13, p<0.05)
exp(0.13)
##Odds Ratio는 Good_Mate 점수가 1점 올라갈 때 무응답에 비해 전화번호를 얻을 확률이 1.14 높아짐
##-------5. Funny:Get Phone Number
##no response와 get phone number를 유의미하게 예측(b=0.49, p<0.001)
exp(0.49)
##Odds Ratio는 Funny 점수가 1점 올라갈 때 전화번호를 얻을 확률이 1.63 높아짐
##-------7. GenderFemale:Get Phone Number
##no response와 get phone number를 유의미하게 예측(b=-1.99, p<0.001)
exp(-1.99)
##Odds Ratio는 남성(0)보다 여성(1)일 때 전화번호를 얻을 확률이 0.14 정도