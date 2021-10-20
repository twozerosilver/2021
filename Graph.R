#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 4 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

getwd()
setwd("C:\\Users\\Owner\\Dropbox\\?ڻ???��\\��??\\2019-2\\????(?к?)_?ø?????\\2. ????????\\05. ?ڷ? ????\\01. 2-5????\\08. IN-CLASS\\????Ȱ???ڷ?\\4??")

######Initiate packages

#If you don't have ggplot2 installed then use:
install.packages(c("ggplot2", "plyr", "reshape"))


#Initiate ggplot2
library(ggplot2)
library(reshape)
library(plyr)


#--------Scatterplots----------

examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
str(examData)
names(examData)

#Simple scatter
scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point()

#Simple scatter / labs(x=??x?? ?̸???, y=??y???̸???)
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %") 

#Simple scatter with smooth/ labs(title = "?׸?��??")
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth() + labs(x = "Exam Anxiety", y = "Exam Performance %", title = "labs(title)")


#Simple scatter with regression line(red)
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Blue", se = F) + labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line(red)+ CI

scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line + coloured CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Red") + labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Grouped scatter with regression line + CI

scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 




##(?л??ǽ��?��)
##"FacebookNarcissism.dat" ?ڷ??? NPQC_R_Total(X??), Rating(y??)?? ?????? ?׸???
## 1) ?????? ?׸???(Rating_Type?? ???? ???? ?ٸ???)
## 2) ???????? ?Բ? ȸ?ͼ? ?׸???(Rating_Type?? ???? ȸ?ͼ? ???? ?ٸ???, method?? lm��??)
## 3) ?׸? title�� "geom_smooth(aes(colour = Rating_Type))" ��?? ��??



#--------HISTOGRAMS----------

##Load the data file into R. This is a tab-delimited file hence use of read.delim

festivalData <- read.delim("DownloadFestival.dat",  header = TRUE)
##Histogram with Outlier
festivalHistogram <- ggplot(festivalData, aes(day1)) + labs(legend.position="none")
festivalHistogram + geom_histogram(binwidth = 0.4) + labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")

#Locate outlier(?߰?)

festivalData<-festivalData[order(festivalData$day1),]


#Density without outlier(?߰?)

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)

festivalDensity <- ggplot(festivalData2, aes(day1))
festivalDensity + geom_density() + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")


festivalDensity + geom_density(aes(fill = gender), alpha = 0.5) + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

#--------BOXPLOTS----------

festivalBoxplot <- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

#with outlier removed

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)
festivalBoxplot2 <- ggplot(festivalData2, aes(gender, day1))
festivalBoxplot2 + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")


##(?л??ǽ��?��) days 2 and 3 ???? box plot ?׷��???##



#--------Bar Charts----------

chickFlick = read.delim("ChickFlick.dat",  header = TRUE)

bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Film", y = "Mean Arousal") 

bar <- ggplot(chickFlick, aes(film, arousal, fill = gender ))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender")


bar <- ggplot(chickFlick, aes(film, arousal, fill = film))
bar + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap(~gender) + labs(x = "Film", y = "Mean Arousal") + theme(legend.position="none")

## ?׸? ???? ?ٸ???!
bar+ stat_summary(fun.y = mean, geom = "bar") + scale_fill_manual(values=c("black", "white"))

levels(chickFlick$film)

library(RColorBrewer)
display.brewer.all()

bar+ stat_summary(fun.y = mean, geom = "bar") +scale_fill_brewer(palette="Oranges")

bar2 <- ggplot(chickFlick, aes(film, arousal, colour= film))
bar2 + geom_point() + scale_colour_brewer(palette="Paired")

scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + scale_colour_brewer(palette="Paired")
scatter + geom_point() + scale_colour_brewer(palette="Oranges")


##(?л??ǽ��?��) chickFlick ?ڷ?, x?? film, y?? arousal.
### 1) ???? ?׷???(???? ???δ? ????, ?׵θ??? ??��??) + ???? ???? errorbar ????, mean_cl_normal?? ???? ǥ??, ???????? ????????��??, ?ʺ??? 0.2)
### 2) ???? ?׷???(???? ???δ? ????, ?׵θ??? ??��??) + ???? ???? errorbar ????, mean_cl_boot?? ???? ǥ??, ???????? ????????��??, ?ʺ??? 0.2)







#--------Line Charts----------

hiccupsData <- read.delim("Hiccups.dat",  header = TRUE)
hiccups<-stack(hiccupsData)
names(hiccups)<-c("Hiccups","Intervention")
hiccups$Intervention_Factor<-factor(hiccups$Intervention, levels(hiccups$Intervention)[c(1, 4, 2, 3)])


line <- ggplot(hiccups,  aes(Intervention_Factor, Hiccups))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1),colour = "Red", linetype = "dashed") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Intervention", y = "Mean Number of Hiccups")

## textdata?? ?߰? ?Ǥ?

textData <- read.delim("TextMessages.dat",  header = TRUE)
textData$id = row(textData[1])

#textMessages = reshape(textData, idvar = c("id", "Group"), varying = c("Baseline", "Six_months"), v.names = "Grammar_Score", timevar = "Time", times = c(0:1), direction = "long")

textMessages<-melt(textData, id = c("id", "Group"), measured = c("Baseline", "Six_months"))
names(textMessages)<-c("id", "Group", "Time", "Grammar_Score")
textMessages$Time<-factor(textMessages$Time, labels = c("Baseline", "6 Months"))

print (textMessages)

## group?? ???? ??�� ?ٸ??? ?׸?

line <- ggplot(textMessages, aes(Time, Grammar_Score, colour = Group))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Group)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Mean Grammar Score", colour = "Group") 


## (?л??ǽ��?��) textMessages??ü?? Time(x), Grammar_Score(y), ?ڷ? ????�� Group?? ???? ?ٸ??? ǥ??
## 1) X?? ???? Y???? ????�� ����?? ǥ??(GROUP?? ???? ��?? ???? ?ٸ???, ��?? ũ???? 4)
 ## *** ��?? ???? ?ٸ??? ?ϴ? ?ɼ? : aes(shape=Group)
## 2) X?? ???? y???? ???? ???̸? ??��?? ??�� : Group ?????? ???? ???? ?ٸ? ?????? ?׷???????, ?? ��???? group?? ???? ?ٸ???,
## 3) ???? ???? ǥ(mean_cl_boot), ?ʺ??? 0.2

## 1) + 2) + 3) ???ļ? ?׸? ?׸???


