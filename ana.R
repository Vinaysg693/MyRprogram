-#day2----------
getwd()
setwd("C:/Users/Kishore/Desktop/data")
a =read.csv("C:/Users/Kishore/Desktop/data/bulb.csv",header = T,sep = ",")
a
View(a)
summary(a)
hist(a$Lifetime_Yrs)
boxplot(a$Lifetime_Yrs)
result1= t.test(a$Lifetime_Yrs,mu=10)
result1

b= read.csv("C:/Users/Kishore/Desktop/data/city_ratings.csv",header = T)
b
hist(b$Rating)
boxplot(b$Rating) 
aggregate(b$Rating,by=list(b$City),summary)
t.test(b$Rating~b$City)

options(scipen = 9999) #to show the exat decimal values

c= read.csv("C:/Users/Kishore/Desktop/data/kitkatsales_anova(1 way).csv",header = T)
c
aov_result= aov(Sales~ as.factor(Place),data = c)
summary(aov_result)

TukeyHSD(aov_result)

aggregate(c,list(c$Place),mean)


#day3--------------
d= read.csv("C:/Users/Kishore/Desktop/ATI_Course/data/students_scores.csv")
d
View(d)
score= t.test(d$Before,d$After,paired = T)
score


library(MASS)    #load the mass packae
View(survey) 
unique(survey$Exer)
unique(survey$Smoke)
tb1 = table(survey$Smoke,survey$Exer)
tb1              # the contingency table
chisq.test(tb1)
#day4----
h= read.csv("C:/Users/Kishore/Desktop/ATI_Course/Machine-Learning-with-R-datasets-master/insurance.csv",header = T, stringsAsFactors = T)
h
View(h)
str(h)
summary(h$charges)
nrow(h[h$charges>34490, ])/nrow(h)
outmax= quantile(h$charges,.75) + 1.5*(IQR(h$charges))
outmax
summary(h)



h$children= as.factor(h$children)
h$children
min(h$charges)

h$charges= log(h$charges)

exp(h$charges)
hist(h$charges)
plot(h$charges)

d= table(h$region)  #chisq.test
d

#exploring relation among features
cor(h[c("age","bmi","children","charges")])
library(psych)

pairs(h[c("age","bmi","children","charges")])
pairs.panels(h[c("age","bmi","children","charges")])

library(caret)
set.seed(3456)
trainIndex = createDataPartition(h$charges, p=.7,list = F, times = 1)
hTrain= h[trainIndex,]  #70%
hTest= h[-trainIndex,]  #30%
View(hTrain)
View(hTest)


#building my 1st model including all the variables
ins_model = lm(charges~ .,data = hTrain)   #linear model
summary(ins_model)
ins_model = lm(charges~ .-1,data = hTrain) #removes inter


ModelPridicted= predict(ins_model,hTest[,-7])
head(ModelPridicted)
head(hTest$charges)

hTest$predict_values= ModelPridicted
View(hTest)
residue = hTest$charges - hTest$predict_values
hist(residue)
class(residue)

#test for auto co relation
dwtest(ins_model)

#test multicolinr
vif(ins_model)
VIF::vif(ins_model)


ins_model = lm(charges~ .,data = hTrain)   #linear model
summary(ins_model)
ins_model = lm(charges~ .-1,data = hTrain) #removes inter


ModelPridicted= predict(ins_model,hTest[,-7])
head(ModelPridicted)
head(hTest$charges)

hTest$predict_values= ModelPridicted
View(hTest)

hTrain$charges= log(hTrain$charges)
hTest$charges= log(hTest$charges)
ins_model = lm(charges ~ age + bmi + children+sex +region+smoker, data = hTrain)   #linear model
summary(ins_model)
ins_model = lm(charges~ .-1,data = hTrain) #removes inter


ModelPridicted= predict(ins_model,hTest[,-7])
head(ModelPridicted)
head(hTest$charges)

hTest$predict_values= ModelPridicted
View(hTest)
residue = hTest$charges - hTest$predict_values
plot(residue)
boxplot(hTest$charges)
