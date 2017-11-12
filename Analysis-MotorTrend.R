library(datasets)
data(mtcars)

str(mtcars) # inspect mtcars
mtcars$am<-factor(mtcars$am,labels=c("Automatic","Manual")) #create factor variables
mtcars$gear<-factor(mtcars$gear)
mtcars$vs<-factor(mtcars$vs)
mtcars$carb<-factor(mtcars$carb)
mtcars$cyl<-factor(mtcars$cyl)

aggregate(mpg~am,data=mtcars,FUN=mean) #summarize mpg by transmission type
boxplot(mpg~am,data=mtcars,xlab="Transmission",ylab="Miles per Gallon",
        main="MotorTrend MPG vs Transmission Type")

t.test(mtcars$mpg~mtcars$am) #test significance

fit_basic<-lm(mpg~am,data=mtcars) #create models
summary(fit_basic)

pairs(mpg ~ ., data = mtcars)

fit_all<-lm(mpg~.,data=mtcars)
fit_best<-step(fit_all,direction="both") #pick best model
summary(fit_best)

confint(fit_best) #test model significance

anova(fit_basic,fit_best)

par(mfrow=c(2,2))    
plot(fit_best)

