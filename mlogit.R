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
chatData<-read.delim("Chat-Up Lines.dat", header = TRUE)
chatData$Gender<-relevel(chatData$Gender, ref = 2)

##-------데이터를 적절한 형식으로 바꾸기
##텍스트 문자열이 포함되어 있으므로 요인여부 확인하고, 필요 시 요인으로 변환하기
##해석을 고려하여 기저범주 설정하기: 현재는 Female이 기저범주. 여성에 대한 작업멘트의 성공 여부가 궁금하다면 기저범주는 male로 두어야 함
##기존의 데이터를 mlogit에 적정한 형태로 바꾸기
head(chatData)
is.factor(chatData$Success)
is.factor(chatData$Gender)
chatData$Success=factor(chatData$Success)
chatData$Gender=factor(chatData$Gender)

str(chatData)
chatData$Gender<-relevel(chatData$Gender,ref=2)

###newDataframe<-mlogit.data(oldDataFrame, choice = "text", shape = "wide"/"long")
mlChat<-mlogit.data(chatData, choice = "Success", shape="wide")
head(mlChat)

##-------다항 로지스틱 회귀분석 모델 만들기
chatModel <- mlogit(Success ~ 1 | Good_Mate + Funny + Gender + Sex + Gender:Sex +  Funny:Gender , data = mlChat, reflevel=3)
summary(chatModel)

data.frame(exp(chatModel$coefficients))
exp(confint(chatModel))



chatBase<-mlogit(Success ~ 1, data = mlChat, reflevel=3)                    
