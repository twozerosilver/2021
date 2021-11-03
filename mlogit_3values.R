##-------다항 로지스틱 회귀분석 R 실습
##-------'작업 멘트'를 평가하는 방식에 관한 연구에서 mlogit으로 분석하기

##-------패키지 설치하기
##다항 로지스틱 회귀분석 시 활용 패키지: mlogit / nnet_multinom
##mlogit으로 분석해보기기
install.packages("car")
install.packages("mlogit")
install.packages("nnet")
install.packages("dfidx")

library(car)
library(mlogit)
library(nnet)
library(dfidx)

##-------디렉토리 설정하기
getwd()

##-------데이터 불러오기
chatData<-read.delim("Chat-Up Lines2.dat", header = TRUE)

##-------데이터를 확인하기
##텍스트 문자열이 포함되어 있으므로 요인여부 확인하고, 필요 시 요인으로 변환하기
##해석을 고려하여 기저범주 설정하기: 현재는 Female이 참조범주
##여성에 대한 작업멘트의 성공 여부가 궁금하다면 참조범주는 Male로 두어야 함

is.na(chatData)
sum(is.na(chatData))

head(chatData, 10)
str(chatData)

chatData$Success=factor(chatData$Success)
chatData$Gender=factor(chatData$Gender)
is.factor(chatData$Success)
is.factor(chatData$Gender)

str(chatData)
levels(chatData$Success)
levels(chatData$Gender)
chatData$Gender<-relevel(chatData$Gender,"Male")
chatData$Success<-relevel(chatData$Success, "No response/Walk Off")

chatData$Success<-factor(chatData$Success, levels = c("No response/Walk Off", 
                                         "Get Phone Number", "Go On A Date"))
levels(chatData$Success)
chatData$Gender<-factor(chatData$Gender, levels = c("Male", "Female"))
levels(chatData$Gender)

##-------기존의 데이터를 mlogit에 적정한 형태로 바꾸기
##newDataframe<-mlogit.data(oldDataFrame, choice = "text", shape = "wide"/"long")
##long layout: 1행 1 관측치, wide layout: 1행 여러 데이터
##각 행이 종속변수(Success)에 대응하여 분리됨
mlChat <- mlogit.data(chatData, choice="Success", shape="wide")
head(mlChat, 10)

##-------다항 로지스틱 회귀분석 모델 만들기
##newDataframe<-mlogit.data(결과변수~예측변수, data = 데이터프레임, na.action = 결측값 처리방식, reflevel = 결과 참조 범주 번호)
##기존 lm이나 glm에 비해 "reflevel"을 해야 한다는 특징
##기저범주 설정하기: No response/Walk off
##예측변수 결정하기: Good_Mate, Funny, Sexual

chatModel <- mlogit(Success ~ 1 | Good_Mate + Funny + Sexual + Gender, 
                    data = mlChat, reflevel = "No response/Walk Off")
summary(chatModel)

##-------회귀모형의 설명력 확인하기
##Log-Likelihood에서 X^=2LL(새모형)-2LL(기본모형)
##이탈도는 카이제곱 분포를 따르므로 유의성을 계산하기 쉬움
chatBase <- mlogit(Success ~ 1, data = mlChat, reflevel = "No response/Walk Off")
summary(chatBase)

##chatBase의 Log-Likelihood: -1008 / chatModel은 Log-Likelihood: -892.02
1008-892.02
(1008-892.02)*-2
##Likelihood ratio test : chisq = 231.95 (p.value = < 2.22e-16)
##전체 모형의 가능도비에 따라 이러한 변화는 유의하다
##chatBase 보다 chatModel이 더 적합하다(많이 설명한다)
##McFadden R^ / pseudo R^ (유사결정계수)
##유사결정계수가 1에 가까운 값을 가질수록 로지스틱 회귀모형의 적합도는 높다고 평가 
##유사결정계수가 0.2보다 큰 값을 갖게 되면 모형은 적합도가 비교적 높은 것으로 판단

lrtest(chatModel, chatBase)
waldtest(chatModel,chatBase)

##-------전화번호를 얻을 확률은 어떻게 변화할까
exp(chatModel$coefficients)
data.frame(exp(chatModel$coefficients))
exp(confint(chatModel))

##-------회귀모형 결과 해석하기
##-------3. Good_Mate:Get Phone Number
##no response와 get phone number를 유의미하게 예측(b=0.13, p<0.05)
exp(0.13)
##Odds Ratio는 Good_Mate 1점 올라갈 때 무응답에 비해 전화번호 얻을 확률이 1.14 높아짐
##-------5. Funny:Get Phone Number
##no response와 get phone number를 유의미하게 예측(b=0.49, p<0.001)
exp(0.49)
##Odds Ratio는 Funny 점수가 1점 올라갈 때 전화번호를 얻을 확률이 1.63 높아짐
##-------7. GenderFemale:Get Phone Number
##no response와 get phone number를 유의미하게 예측(b=-1.99, p<0.001)
exp(-1.99)
##Odds Ratio는 남성(0)보다 여성(1)일 때 전화번호를 얻을 확률이 0.14 정도
##여성보다 남성일 때 전화번호를 얻을 확률이 614.29% 증가

##------발생 확률 구하기
head(fitted(chatModel, outcome = FALSE))
apply(fitted(chatModel, outcome = FALSE), 2, mean)

predict(chatModel, newdata=test1 , type="response")
test1 <- data.frame(Gender = c("Female", "Male"), 
                   Sexual = mean(chatData$Sexual), 
                   Good_Mate = mean(chatData$Good_Mate), 
                   Funny = mean(chatData$Funny))
test1.pre <- predict(mlChat, newdata = test1, type = "response")

##-------[참고] nnet_multinom 활용
##다항 로짓스틱 회귀분석 모델 만들기
##newDataframe<-multinom(결과변수~예측변수, data = 본 데이터)
chatModel2 <- multinom(Success ~ 
                         Good_Mate + Funny + Sexual + Gender, data = chatData)
summary(chatModel2)
exp(coef(chatModel2))
##변수의 유의성 검정(Z검정)
z <- summary(chatModel2)$coefficients/summary(chatModel2)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
##성별에 따른 예측값의 변화
FtoM <- data.frame(Gender = c("Female", "Male"), 
                   Sexual = mean(chatData$Sexual), 
                   Good_Mate = mean(chatData$Good_Mate), 
                   Funny = mean(chatData$Funny))
FtoM
predict(chatModel2, newdata = FtoM, "probs")