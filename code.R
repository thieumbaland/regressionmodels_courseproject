library(dplyr)
library(ggplot2)

data(mtcars)

str(mtcars)
mtcars$cyl  <- factor(mtcars$cyl)
mtcars$vs   <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am   <- factor(mtcars$am,labels=c("Automatic","Manual"))
summary(mtcars)

#manual vs. automatic
ggplot(mtcars,aes(x=as.factor(am),y=mpg))+geom_boxplot()+theme_bw()
t.test(mpg~am,data=mtcars)


#regression
model.allvars<-lm(mpg ~ .,data=mtcars)
?step
model.final<-step(model.allvars,direction="both")
summary(model.final)

cor(mtcars$hp,mtcars$wt)
cor(mtcars$hp,as.numeric(mtcars$cyl))
cor(mtcars$wt,as.numeric(mtcars$cyl))

model.revised<-lm(mpg ~ hp+am,data=mtcars)
summary(model.revised)

anova(model.revised,model.final)
