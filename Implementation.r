databike <- read.csv("C:\\Users\\Lenovo\\OneDrive\\Υπολογιστής\\BSc_BusinessAnalytics\\Statistics_for_Business_AnalyticsI\\Εργασία_Μεγάλη\\bike_data_21-22\\bike_14.csv",sep=";")
View(databike)
str(databike)

#Τask1
#Check for NA's in the dataset
missing_val<-data.frame(apply(databike,2,function(x){sum(is.na(x))}))
names(missing_val)[1]='missing_val'

#change the class of the variables
databike$dteday <- as.Date(databike$dteday)
databike$season <- as.factor(databike$season)
databike$yr <- as.factor(databike$yr)
databike$holiday <- as.factor(databike$holiday)
databike$mnth <- as.factor(databike$mnth)
databike$hr <- as.factor(databike$hr)
databike$weekday <- as.factor(databike$weekday)
databike$workingday <- as.factor(databike$workingday)
databike$weathersit <- factor(databike$weathersit, labels =c('Good','Medium','Bad','Very Bad'))
databike$temp <-  as.numeric(gsub(",", ".",databike$temp)) * 41
databike$atemp <- as.numeric(gsub(",", ".",databike$atemp)) * 50
databike$hum <- as.numeric(gsub(",", ".",databike$hum)) * 100
databike$windspeed <- as.numeric(gsub(",", ".",databike$windspeed)) * 67
databike$casual <- as.numeric(databike$casual)
databike$registered <- as.numeric(databike$registered)
databike$cnt <- as.numeric(databike$cnt)

summary(databike)

#remove the variables that are not usefull for the model
databikecleaned <- databike[,-c(1,2,3,16,17)] 

View(databikecleaned)
str(databikecleaned)
summary(databikecleaned)

numcolumns <- sapply(databikecleaned, class) == 'numeric'  #choose only the numeric variables
bikesnum <- databikecleaned[ ,numcolumns]


#check the correlation between the  numeric variables
pairs(bikesnum)
round(cor(bikesnum),2)
library(corrplot)
corrplot(cor(bikesnum), method= "number")
library(corrgram)
corrgram(bikesnum, lower.panel=panel.shade, upper.panel=panel.cor)

#analysis 
hist(bikesnum[,1], col = 'green', main = "Θερμοκρασίες- Συχνότητα ", xlab = "Θερμοκρασίες", ylab = "Συχνότητα")
hist(bikesnum[,2], col = 'green', main = "Αίσθησης Θερμοκρασιών - Συχνότητα " , xlab = "Αίσθηση Θερμοκρασιών", ylab = "Συχνότητα")
hist(bikesnum[,3], col = 'green', main = "Υγρασία- Συχνότητα" , xlab = "Υγρασία", ylab = "Συχνότητα")
hist(bikesnum[,4], col = 'green', main = "Ταχήτητα του αέρα - Συχνότητα" , xlab = "Ταχήτητα του αέρα", ylab = "Συχνότητα")
hist(databike$cnt, breaks = 30, col = 'green', prob=TRUE, main = "Ιστόγραμμα Σύνολο ενοικιάσεων - Συχνότητα ", xlab = "Σύνολο ενοικιάσεων", ylab = "Συχνότητα") # plot histrogram of target variable
lines(density(databike$cnt))

boxplot(databikecleaned$season)
boxplot(databikecleaned$yr)
boxplot(databikecleaned$mnth)
boxplot(databikecleaned$hr)
boxplot(databikecleaned$holiday)
boxplot(databikecleaned$weekday)
boxplot(databikecleaned$workingday)
boxplot(databikecleaned$weathersit)


ggplot(databikecleaned, aes(x = season, y = cnt, fill = factor(season))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='Εποχή',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων ανά Εποχή') + theme_bw()

ggplot(databikecleaned, aes(x = yr, y = cnt, fill = factor(yr))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='Χρόνια',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων ανά Χρονιά') + theme_bw()

ggplot(databikecleaned, aes(x = mnth, y = cnt, fill = factor(mnth))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='Μήνες',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων ανά Μήνα') + theme_bw()

ggplot(databikecleaned, aes(x = hr, y = cnt, fill = factor(hr))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='Ώρες',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων ανά Ώρα') + theme_bw()

ggplot(databikecleaned, aes(x = holiday, y = cnt, fill = factor(holiday))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='Διακοπές',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων για περιόδους διακοπών και μη') + theme_bw()

ggplot(databikecleaned, aes(x = weekday, y = cnt, fill = factor(weekday))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='Ημέρα της εδβομάδας',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων ανα Ημέρα') + theme_bw()

ggplot(databikecleaned, aes(x = workingday, y = cnt, fill = factor(workingday))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='Εργάσιμη ημέρα ή μη',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων για εργάσιμες και μη ημέρες') + theme_bw()

ggplot(databikecleaned, aes(x = weathersit, y = cnt, fill = factor(weathersit))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='καιρικές συνθήκες',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων ανα καιρικές συνθήκες') + theme_bw()


#Διάγραμμα1
#box plots for outliers
par(mfrow=c(2,2))
#Box plot for temp outliers
boxplot(databikecleaned$temp, main="Θερμοκρασίες",sub=paste(boxplot.stats(databikecleaned$temp)$out))
#Box plot for humidity outliers
boxplot(databikecleaned$hum,main="Υγρασία",sub=paste(boxplot.stats(databikecleaned$hum)$out))
#Box plot for windspeed outliers
boxplot(databikecleaned$windspeed,main="Ταχήτητα του αέρα",sub=paste(boxplot.stats(databikecleaned$windspeed)$out))

library(ggplot2)

#Διάγραμμα2
ggplot(databikecleaned,aes(x=yr,y=cnt,fill=yr))+geom_violin() + theme_bw() +
  labs(x='Έτος',y='Συνολικές Ενοικιάσεις',title='Συνολικές Ετήσιες ενοικιάσεις')

#Διάγραμμα3
ggplot(databikecleaned, aes(x = hr, y = cnt, fill = factor(workingday))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='Ώρες',y='Συνολικές Ενοικιάσεις',title='Συνολικές ενοικιάσεις για εργάσιμες και μη ημέρες ανα ώρα') + 
  theme_bw() 

#Διάγραμμα4
ggplot(databikecleaned,aes(x=hr,y=cnt, fill=weekday))+theme_bw( )+
  stat_summary(fun=mean, aes(group=1), geom="point", colour="blue")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="blue")+
  labs(x='Ώρα',y='Συνολικές Ενοικιάσεις',title='Μέσος όρος συνολικών ενοικιάσεων ανά ώρα') #weekly total 

#Διάγραμμα5
ggplot(databikecleaned, aes(x = hr, y = cnt, fill = factor(hr))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  ylab("Σύνολο Ενοικιάσεων") +theme_bw() +
  ggtitle("\n")

#Διάγραμμα6
ggplot(databikecleaned,aes(x=workingday,y=cnt,fill=season))+geom_col()+theme_bw()+
  labs(x='Εργάσιμες και μη εργάσιμες ημέρες',y='Σύνολο Ενοικιάσεων',title='Σύνολο Ενοικιάσεων για εργάσημές και μη εργάσιμες ημέρες ανα εποχή')

#Διάγραμμα7
ggplot(databikecleaned,aes(x=weekday,y=cnt, fill=season))+theme_bw( )+geom_col()+
  labs(x='Ημέρα της εβδομάδας',y='Σύνολο Ενοικιάσεων',title='Σύνολο Ενοικιάσεων για κάθε ημέρα της εβδομάδας και ανα εποχή')
#ggplot(databikecleaned,aes(x=holiday,y=cnt,fill=season))+geom_col()+theme_bw()+
#  labs(x='holiday',y='Total_Count',title='Holiday wise distribution of counts')

#Διάγραμμα8
ggplot(databikecleaned,aes(x=mnth,y=cnt, fill=weekday))+theme_bw( )+
  stat_summary(fun=mean, aes(group=1), geom="point", colour="blue")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="blue")+
  labs(x='Μήνες',y='Σύνολο Ενοικιάσεων',title='Σύνολο Ενοικιάσεων για κάθε μήνα') 

#Διάγραμμα9
nonhol = subset(databikecleaned, holiday == 0)
summary(nonhol$cnt)
hol = subset(databikecleaned, holiday == 1)
summary(hol$cnt)
boxplot(x=nonhol$cnt,hol$cnt,names = c("όχι ημέρες διακοπών","ημέρες διακοπών"),col="#33658A",
        ylab="Σύνολο ενοικιάσεων",xlab="Είδος ημέρας", main = "Σύγκρηση συνόλου ενοικιάσεων για ημέρες διακοπών και μη",
        cex.main = 2, cex.lab = 1.5, cex.axis = 1.3)

#Διάγραμμα10
ggplot(databikecleaned,aes(x=weathersit,y=cnt,fill=season))+geom_col()+theme_bw()+
  labs(x='Καιρικές Συνθήκες',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων ανα κατηγορία καιρικών συνθηκών και ανά εποχή')

#Διάγραμμα11
ggplot(databikecleaned,aes(x=season,y=cnt,fill=season))+theme_bw( )+geom_col()+
  labs(x='Εποχή',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων ανα εποχή') 

#Διάγραμμα12
ggplot(databikecleaned, aes(x = season, y = cnt, fill = factor(season))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  labs(x='Εποχή',y='Σύνολο ενοικιάσεων',title='Σύνολο Ενοικιάσεων ανά εποχή') + theme_bw() +
  ggtitle("\n") +
  scale_fill_manual(values = c("#55DDE0",  "#F6AE2D", "#F26419", "#33658A", "#2F4858", "#999999"), 
                    name="Εποχή:",
                    breaks=c(1, 2, 3, 4),
                    labels=c("Χειμώνας", "’νοιξη", "Καλοκαίρι","Φθινόπωρο"))+
  theme(text = element_text(size = 20), plot.title = element_text(size = 24, face="bold"))

#Διάγραμμα13
plot <- ggplot(databikecleaned, aes(atemp, cnt)) + geom_smooth(aes(color = cnt))
plot + labs(x='Ποσούς βαθμούς κελσίου αισθάνεσαι',y='Συνολικές ενοικιάσεις',title='Σύνολο Ενοικιάσεων ανά αίσθηση βαθμών κελσίου ') +
  theme_light(base_size = 11) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme(text = element_text(size = 20), plot.title = element_text(size = 24, face="bold"))

#Διάγραμμα14
ggplot(databikecleaned, aes(x = hum, y = cnt, color = weathersit)) +
  geom_smooth(method = 'auto', size = 1) +
  theme_light(base_size = 11) +
  xlab("Υγρασία") +
  ylab("Σύνολο Ενοικιάσεων") +
  ggtitle("\n") +
  theme(plot.title = element_text(size = 11, face="bold")) +
  theme(text = element_text(size = 20), plot.title = element_text(size = 24, face="bold"))

#Διάγραμμα15
library(gridExtra)
p21 = ggplot(databike,aes(x=weekday,y=casual))+theme_bw( )+
  stat_summary(fun=mean, aes(group=1), geom="point", colour="blue")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="blue")+
  labs(x='Ημέρα της εβδομάδας',y='Περιστασιακοί χρήστες',title='Μέσος όρος ενοικιάσεων από περιστασιακούς χρήστες') 
# Avg Use of the bikes by registered users
p22 = ggplot(databike,aes(x=weekday,y=registered))+theme_bw( )+
  stat_summary(fun=mean, aes(group=1), geom="point", colour="blue")+
  stat_summary(fun=mean, aes(group=1), geom="line", colour="blue")+
  labs(x='Ημέρα της εβδομάδας',y='Εγγεγραμμένοι χρήστες',title='Μέσος όρος ενοικιάσεων από εγγεγραμμένους χρήστες') 
# arrange plots
grid.arrange(p21,p22, ncol=2)



#task2
#implement lasso
fullmodel <- lm(cnt~.  , data = databikecleaned)
summary(fullmodel)

require(glmnet)
X <- model.matrix(fullmodel)[,-1]
lasso <- glmnet(X, databikecleaned$cnt)
plot(lasso, xvar = "lambda", label = T)

lasso <- cv.glmnet(X, databikecleaned$cnt, alpha = 1)
lasso$lambda
lasso$lambda.min
lasso$lambda.1se
plot(lasso)
coef(lasso, s = "lambda.min")
coef(lasso, s = "lambda.1se")
plot(lasso$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso$lambda.min, lasso$lambda.1se)), lty =2)

#implement stepwise for the model without the variables that have been excluded from lasso (10 independent remain)
model <-  lm(cnt~ temp + hum + windspeed + season + yr + mnth + hr + holiday  + workingday + weathersit, data = databikecleaned)
summary(model)

summary(step(model, direction="both")) #AIC
newmodel <- lm(formula = cnt ~ temp + hum + windspeed + season + yr + mnth + hr + workingday + weathersit, data = databikecleaned)
summary(newmodel)

#Check for multicollinearity
require(car)
round(vif(newmodel),1)
vif(step(newmodel, direction = "both"))
mstep <- step(newmodel, direction = "both")
vif(update(mstep,.~. - X1)) 

#exctract the mnth VIF = 387.3
newmodel1 <- lm(formula = cnt ~ temp + hum + windspeed + season + yr  + hr + workingday + weathersit, data = databikecleaned)

round(vif(newmodel1),1)
vif(step(newmodel1, direction = "both"))
mstep <- step(newmodel1, direction = "both")
vif(update(mstep,.~. - X1)) 

#exctract the season VIF= 3.44
newmodel2 <- lm(formula = cnt ~ temp + hum + windspeed +  yr  + hr + workingday + weathersit, data = databikecleaned)#1/4 assumptions

round(vif(newmodel2),1)
vif(step(newmodel2, direction = "both"))
mstep <- step(newmodel2, direction = "both")
vif(update(mstep,.~. - X1)) 

#Check the assumptions

#Normality
hist(newmodel2$residuals, probability=T, main='Residuals')
x0<-seq( min(newmodel2$residuals), max(newmodel2$residuals),
         length.out=100)
y0<-dnorm( x0, mean(newmodel2$residuals),
           sd(newmodel2$residuals) )
lines(x0,y0, col=2, lty=2)
qqnorm(newmodel2$residuals, main='Residuals')
qqline(newmodel2$residuals)

library(nortest)
lillie.test(newmodel2$residuals) #because we have a sample with length greater than 50.
shapiro.test(newmodel2$residuals)

#Constant variance(Homoscedasticity)
Stud.residuals<-rstudent(newmodel2)
yhat <- fitted(newmodel2)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)

plot(yhat, Stud.residuals^2)

abline(h=4, col=2, lty=2)

yhat <- fitted(newmodel2)
qyhat<- cut(yhat, breaks= 4)
library(car)
ncvTest(newmodel2)

#Independence of errors
plot(rstudent(newmodel2), type='l')

library(randtests)
runs.test(newmodel2$residuals)

library(car)
durbinWatsonTest(newmodel2)
dwt(newmodel2)

#Liniarity
library(car)
residualPlot(newmodel2, type='rstudent')
plot(newmodel2$fit, newmodel2$res)
abline(h=0, lty=3)
lines(lowess(newmodel2$fit,newmodel2$res), col=2)

yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
boxplot(rstudent(newmodel2)~yhat.quantiles)
abline(h=0, lty=3)

residualPlots(newmodel2, plot = F)
########################################################################################################
#Transfrormations and 
#1st try: log(cnt)
logmodel1 <- lm(formula = log(cnt) ~ temp + hum + windspeed +  yr  + hr + workingday + weathersit, data = databikecleaned) #2/4 assumptions
summary(logmodel1)

#2nd try weight = w  MY FINAL MODEL
w <- 1/lm(abs(logmodel1$residuals)~logmodel1$fitted.values)$fitted.values^2
logmodel1 <- lm(formula = log(cnt) ~ temp + hum + windspeed +  yr  + hr + workingday + weathersit, weight = w, data = databikecleaned)#3/4 assumptions
#Check the assumptions

#Normality
hist(logmodel1$residuals, probability=T, main='Residuals')
x0<-seq( min(logmodel1$residuals), max(logmodel1$residuals),
         length.out=100)
y0<-dnorm( x0, mean(logmodel1$residuals),
           sd(logmodel1$residuals) )
lines(x0,y0, col=2, lty=2)
qqnorm(logmodel1$residuals, main='Residuals')
qqline(logmodel1$residuals)

library(nortest)
lillie.test(logmodel1$residuals) #because we have a sample with length greater than 50.
shapiro.test(logmodel1$residuals)

#Constant variance(Homoscedasticity)
Stud.residuals<-rstudent(logmodel1)
yhat <- fitted(logmodel1)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)

plot(yhat, Stud.residuals^2)

abline(h=4, col=2, lty=2)

yhat <- fitted(logmodel1)
qyhat<- cut(yhat, breaks= 4)
library(car)
ncvTest(logmodel1)

#Independence of errors
plot(rstudent(logmodel1), type='l')

library(randtests)
runs.test(logmodel1$residuals)

library(car)
durbinWatsonTest(logmodel1)
dwt(logmodel1)

#Liniarity
library(car)
residualPlot(logmodel1, type='rstudent')
plot(logmodel1$fit, logmodel1$res)
abline(h=0, lty=3)
lines(lowess(logmodel1$fit,logmodel1$res), col=2)

yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
boxplot(rstudent(logmodel1)~yhat.quantiles)
abline(h=0, lty=3)

#Final Model
logmodel1 <- lm(formula = log(cnt) ~ temp + hum + windspeed +  yr  + hr + workingday + weathersit, weight = w, data = databikecleaned)#3/4 assumptions
summary(logmodel1)



#Task 5
library(Metrics)

train_data <- databike
test_data <- read.csv("C:\\Users\\Lenovo\\OneDrive\\Υπολογιστής\\BSc_BusinessAnalytics\\Statistics_for_Business_AnalyticsI\\Εργασία_Μεγάλη\\bike_data_21-22\\bike_test.csv",sep=";")
View(train_data)
View(test_data)

dim(train_data)
dim(test_data)


nullmodel <- lm(formula = cnt ~ 1 , data = databikecleaned)
fullmodel <- lm(formula = cnt ~ . , data = databikecleaned)
logmodel <- lm(formula = log(cnt) ~ 1 + yr + hr + weathersit + 
                 temp + hum + I(hum^2) + I(temp^2) ,weight = w, data = databikecleaned)

mae(test_data$cnt, predict(nullmodel))
mae(test_data$cnt, predict(fullmodel))
mae(test_data$cnt, predict(logmodel))  

#predict <- predict(logmodel,test_data)
#rmse(predict, test_data[,18])


#Task6

s1 = subset(databike, season == 1)
summary(s1)

s2 = subset(databike, season == 2)
summary(s2)

s3 = subset(databike, season == 3)
summary(s3)

s4 = subset(databike, season == 4)
summary(s4)

