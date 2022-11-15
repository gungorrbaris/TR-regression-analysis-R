## ----setup, include=FALSE---------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------
ham_veri <- read.csv("C:/Users/gungo/Desktop/regresyon/ham_veri.txt", sep="")
site_tasarimi<- as.factor(ham_veri$site_tasarimi)
head(ham_veri)


## ---------------------------------------
attach(ham_veri)
names(ham_veri)



## ---------------------------------------
library(pastecs)
stat.desc(ham_veri)
summary(site_tasarimi)


## ---------------------------------------
qqnorm(y,main = "Müşteri etkinliği için Q-Q Plot")
qqline(y)


## ---------------------------------------
library(nortest)
lillie.test(y)


## ---------------------------------------
ln_y <- log(y)



## ---------------------------------------
qqnorm(ln_y,main = "Müşteri etkinliği için (Lny) Q-Q Plot")
qqline(ln_y)


## ---------------------------------------
shapiro.test(ln_y)
ad.test(ln_y)
lillie.test(ln_y)


## ---------------------------------------
y2 <- 1/y


## ---------------------------------------
qqnorm(y2,main = "Müşteri etkinliği için (1/y) Q-Q Plot")
qqline(y2)


## ---------------------------------------
lillie.test(y2)


## ---------------------------------------
yeni_veri <- cbind(y2,yapay_zeka,indirimli_urun,bildirim,site_tasarimi)


## ---------------------------------------
pairs(yeni_veri,col="darkgreen", main="Müşteri Etkinliği (y) için Saçılım Grafiği")


## ---------------------------------------
model <- lm(y2 ~ yapay_zeka + indirimli_urun + bildirim + site_tasarimi)
summary(model)

## ---------------------------------------
confint(model,level = .95)


## ---------------------------------------
inf <- ls.diag(model)
plot(predict(model), (inf$stud.res), ylab="Studentized Residuals", xlab="Predicted Value",pch="*",cex=2)
abline(h=-3, col="darkred")

which(inf$stud.res<(-3))


## ---------------------------------------
n <- length(y2)
k <- length(ham_veri)-1

cooksd <- cooks.distance(model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance",,col=ifelse(cooksd>4/n,'red','green'))
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="darkred")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""), col="black",pos = 1) 


## ---------------------------------------
library(zoo)

hat <- inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value",col=ifelse(hat>2*(k+1)/n,'red','green'))
abline(h = 2*(k+1)/n , col="darkred")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/n,index(hat),""), col="black",pos = 1)


## ---------------------------------------
par(mfrow=c(2,1))

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance",,col=ifelse(cooksd>4/n,'red','green'))
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="darkred")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""), col="black",pos = 1) 

plot(hat, pch="*", cex=2, main="Leverage Value by Hat value",col=ifelse(hat>2*(k+1)/n,'red','green'))
abline(h = 2*(k+1)/n , col="darkred")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/n,index(hat),""), col="black",pos = 1)


## ---------------------------------------
son_veri <- read.csv("C:/Users/gungo/Desktop/regresyon/son.txt", sep="")
site_tasarimi <- as.factor(son_veri$site_tasarimi)
attach(son_veri)
names(son_veri)



## ---------------------------------------
qqnorm(y3,main = "Artıksız Müşteri etkinliği için (y3) Q-Q Plot")
qqline(y3)

## ---------------------------------------
library(nortest)
lillie.test(y3)


## ---------------------------------------
pairs(son_veri,col="darkgreen", main="Artıksız Müşteri Etkinliği(y3) için Saçılım Grafiği")


## ---------------------------------------
model2 <- lm(y3 ~ yapay_zeka + indirimli_urun + bildirim + site_tasarimi)
summary(model2)


inf2 <- ls.diag(model2)
plot(predict(model2), abs(inf2$stud.res), ylab="Studentized Residuals", xlab="Predicted Value")
abline(h=-3, col="darkred")


which(inf2$stud.res<(-3))


## ---------------------------------------
n2 <- length(y3)
k2 <- length(son_veri)-1

cooksd2 <- cooks.distance(model2)
plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance",,col=ifelse(cooksd2>4/n,'red','green'))
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="darkred")
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>if (n>50) 4/n else 4/(n-k-1),names(cooksd2),""), col="black",pos = 1) 



## ---------------------------------------
library(zoo)
hat2 <- inf2$hat
plot(hat2, pch="*", cex=2, main="Leverage Value by Hat value",col=ifelse(hat2>2*(k2+1)/n2,'red','green'))
abline(h = 2*(k2+1)/n2 , col="darkred")
text(x=1:length(hat2)+1, y=hat2, labels=ifelse(hat2>2*(k2+1)/n2,index(hat2),""), col="black",pos = 1)



## ---------------------------------------
par(mfrow=c(2,1))

plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance",,col=ifelse(cooksd2>4/n,'red','green'))
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="darkred")
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>if (n>50) 4/n else 4/(n-k-1),names(cooksd2),""), col="black",pos = 1)


plot(hat2, pch="*", cex=2, main="Leverage Value by Hat value",col=ifelse(hat2>2*(k2+1)/n2,'red','green'))
abline(h = 2*(k2+1)/n2 , col="darkred")
text(x=1:length(hat2)+1, y=hat2, labels=ifelse(hat2>2*(k2+1)/n2,index(hat2),""), col="black",pos = 1)


## ---------------------------------------
model2 <- lm(y3 ~ yapay_zeka + indirimli_urun + bildirim + site_tasarimi)
summary(model2)


## ---------------------------------------
model2 <- lm(y3 ~ yapay_zeka + indirimli_urun + bildirim + site_tasarimi+yapay_zeka*site_tasarimi+indirimli_urun*site_tasarimi+bildirim*site_tasarimi)
summary(model2)


## ---------------------------------------
model2 <- lm(y3 ~ yapay_zeka + indirimli_urun + bildirim + site_tasarimi)
summary(model2)


## ---------------------------------------
confint(model2,level = .99)


## ---------------------------------------
plot(predict(model2),inf2$stud.res , ylab="Studentized Residuals", xlab="Predicted Value")


## ---------------------------------------
library(lmtest)
bptest(model2)


## ---------------------------------------
dwtest(model2)

## ---------------------------------------
attach(son_veri)


## ---------------------------------------
library(DAAG)
vif(model2)

library(perturb)
colldiag(model.matrix(model2),add.intercept=FALSE)


## ---------------------------------------
library(fastDummies)

dummy <- dummy_cols(site_tasarimi)

site_tasarimi1 <- dummy$.data_1
site_tasarimi2 <- dummy$.data_2
site_tasarimi3 <- dummy$.data_3

ort1<-mean(yapay_zeka) 
kt1<-sum((yapay_zeka-ort1)^2) 
skx1<-(yapay_zeka-ort1)/(kt1^0.5) 

ort2<-mean(indirimli_urun) 
kt2<-sum((indirimli_urun-ort2)^2) 
skx2<-(indirimli_urun-ort2)/(kt2^0.5) 

ort3<-mean(bildirim) 
kt3<-sum((bildirim-ort3)^2) 
skx3<-(bildirim-ort3)/(kt3^0.5) 

ort_site2<-mean(site_tasarimi2) 
kt_site2<-sum((site_tasarimi2-ort_site2)^2) 
skx_site2<-(site_tasarimi2-ort_site2)/(kt_site2^0.5)

ort_site3<-mean(site_tasarimi3) 
kt_site3<-sum((site_tasarimi3-ort_site3)^2) 
skx_site3<-(site_tasarimi3-ort_site3)/(kt_site3^0.5) 

x<-cbind(skx1,skx2,skx3,skx_site2,skx_site3) 
sm<- eigen (t(x)%*%x) 

signif(sm$values,3) 

signif(sm$vectors,3)


## ---------------------------------------
predict(model2, newdata = data.frame(yapay_zeka = 10.56, indirimli_urun= 0.497, bildirim= 3.16, site_tasarimi="2"), interval = "confidence")

## ---------------------------------------
predict(model2, newdata = data.frame(yapay_zeka = 25.789, indirimli_urun= 0.657, bildirim= 4.332, site_tasarimi="3"), interval = "confidence")


## ---------------------------------------
library(stats)
lm.null <- lm(y3 ~ 1)
forward <- step(lm.null,y3~yapay_zeka + indirimli_urun +bildirim+site_tasarimi,  direction = "forward")
forward
summary(forward)


## ---------------------------------------
backward<-step(model2,direction="backward")
backward
summary(backward) 

## ---------------------------------------
library(MASS)

step.model <- stepAIC(model2, direction = "both", trace = FALSE)

summary(step.model)


## ---------------------------------------
library(MASS)

ridge <- lm.ridge(model2 ,lambda = seq(0,1,0.05))

matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),

ylab=expression(hat(beta)))

abline(h=0,lwd=2)

ridge$coef

select(ridge)


