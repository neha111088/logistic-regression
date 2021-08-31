attach(bank.full)

sum(is.na(bank.full))

boxplot(age)$out

summary(bank.full)

str(bank.full)

bank.fullfinal <- bank.full[ ,-c(1,2,3,4,10,11,14,15)]
View(bank.fullfinal)
attach(bank.fullfinal)

summary(bank.fullfinal)

View(bank.fullfinal)
attach(bank.fullfinal)

install.packages("dummies")
library(dummies)

bank.fullfinal_1<-dummy.data.frame(bank.fullfinal)
View(bank.fullfinal_1)





summary(bank.fullfinal_1)


bank.fullfinal_1$defaultno <-factor(bank.fullfinal_1$defaultno)
str(bank.fullfinal_1$defaultno)


bank.fullfinal_1$defaultyes <- factor(bank.fullfinal_1$defaultyes)
str(bank.fullfinal_1$defaultyes)


bank.fullfinal_1$housingno <- factor(bank.fullfinal_1$housingno)
str(bank.fullfinal_1$housingno)


bank.fullfinal_1$housingyes <- factor(bank.fullfinal_1$housingyes)
str(bank.fullfinal_1$housingyes)


bank.fullfinal_1$loanno <- factor(bank.fullfinal_1$loanno)
str(bank.fullfinal_1$loanno)



bank.fullfinal_1$loanyes <- factor(bank.fullfinal_1$loanyes)
str(bank.fullfinal_1$loanyes)


bank.fullfinal_1$contactcellular <- factor(bank.fullfinal_1$contactcellular)
str(bank.fullfinal_1$contactcellular)


bank.fullfinal_1$contacttelephone <- factor(bank.fullfinal_1$contacttelephone)
str(bank.fullfinal_1$contacttelephone)


bank.fullfinal_1$contactunknown <- factor(bank.fullfinal_1$contactunknown)
str(bank.fullfinal_1$contactunknown)


bank.fullfinal_1$poutcomefailure <- factor(bank.fullfinal_1$poutcomefailure)
str(bank.fullfinal_1$poutcomefailure)


bank.fullfinal_1$poutcomeother <- factor(bank.fullfinal_1$poutcomeother)
str(bank.fullfinal_1$poutcomeother)


bank.fullfinal_1$poutcomesuccess <- factor(bank.fullfinal_1$poutcomesuccess)
str(bank.fullfinal_1$poutcomesuccess)


bank.fullfinal_1$poutcomeunknown <- factor(bank.fullfinal_1$poutcomeunknown)
str(bank.fullfinal_1$poutcomesuccess)


bank.fullfinal_1$yno <- factor(bank.fullfinal_1$yno)
str(bank.fullfinal_1$yno)


bank.fullfinal_1$yyes <- factor(bank.fullfinal_1$yyes)
str(bank.fullfinal_1$yyes)




str(bank.fullfinal_1)



logistic <- glm(yno~. , family = "binomial" , data = bank.fullfinal_1)
summary(logistic)


logistic1 <- glm(yyes~. , family = "binomial" , data = bank.fullfinal_1)
summary(logistic1)


prob <-predict(logistic,type = c("response"),bank.fullfinal_1)
prob


prob1 <-predict(logistic1,type = c("response"),bank.fullfinal_1)
prob1


confusion <-table(prob>0.50,bank.fullfinal_1$yno)
confusion
table(bank.fullfinal_1$yno)

confusion1 <-table(prob>0.50,bank.fullfinal_1$yyes)
confusion1
table(bank.fullfinal_1$yyes)

accuracy <- sum(diag(confusion)/sum(confusion))
accuracy

accuracy1 <- sum(diag(confusion1)/sum(confusion1))
accuracy1


install.packages("ROCR")
library(ROCR)


rocrpred <- prediction(prob,bank.fullfinal_1$yno)
rocrperf <- performance(rocrpred,'tpr','fpr')
str(rocrperf)


rocrpred1 <- prediction(prob1,bank.fullfinal_1$yyes)
rocrperf1 <- performance(rocrpred1,'tpr','fpr')
str(rocrperf1)



window()
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7,print.cutoffs.at=seq(0.1,by=0.1)))

plot(rocrperf1,colorize=T,text.adj=c(-0.2,1.7,print.cutoffs.at=seq(0.1,by=0.1)))


rocr_cutoff <- data.frame(cut_off=rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr = rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","fpr","tpr")
rocr_cutoff  <- round(rocr_cutoff ,2)


rocr_cutoff1 <- data.frame(cut_off=rocrperf1@alpha.values[[1]],fpr=rocrperf1@x.values,tpr = rocrperf1@y.values)
colnames(rocr_cutoff1) <- c("cut_off1","fpr","tpr")
rocr_cutoff1  <- round(rocr_cutoff1 ,2)
