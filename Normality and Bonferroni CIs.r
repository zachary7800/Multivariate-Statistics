setwd("~/730")
wine <- read.csv("wineFl2021.csv")

#Question 1
#A
hist(wine$fixed_acidity)
qqnorm(wine$fixed_acidity)
qqline(wine$fixed_acidity, col = 'blue')

shapiro.test(wine$fixed_acidity)

hist(wine$volatile_acidity)
qqnorm(wine$volatile_acidity)
qqline(wine$volatile_acidity, col = 'blue')

shapiro.test(wine$volatile_acidity)

hist(wine$citric_acid)
qqnorm(wine$citric_acid)
qqline(wine$citric_acid, col = 'blue')

shapiro.test(wine$citric_acid)

hist(wine$residual_sugar)
qqnorm(wine$residual_sugar)
qqline(wine$residual_sugar, col = 'blue')

shapiro.test(wine$residual_sugar)

hist(wine$chlorides)
qqnorm(wine$chlorides)
qqline(wine$chlorides, col = 'blue')

shapiro.test(wine$chlorides)

hist(wine$free_sulfur_dioxide)
qqnorm(wine$free_sulfur_dioxide)
qqline(wine$free_sulfur_dioxide, col = 'blue')

shapiro.test(wine$free_sulfur_dioxide)

hist(wine$total_sulfur_dioxide)
qqnorm(wine$total_sulfur_dioxide)
qqline(wine$total_sulfur_dioxide, col = 'blue')

shapiro.test(wine$total_sulfur_dioxide)

hist(wine$density)
qqnorm(wine$density)
qqline(wine$density, col = 'blue')

shapiro.test(wine$density)

hist(wine$pH)
qqnorm(wine$pH)
qqline(wine$pH, col = 'blue')

shapiro.test(wine$pH)

hist(wine$sulphates)
qqnorm(wine$sulphates)
qqline(wine$sulphates, col = 'blue')

shapiro.test(wine$sulphates)

hist(wine$alcohol)
qqnorm(wine$alcohol)
qqline(wine$alcohol, col = 'blue')

shapiro.test(wine$alcohol)

#B and C
n=nrow(wine)
xbar=colMeans(wine[,1:11])
s=var(wine[,1:11])
invs= solve(s)
d=rep(0,n)

for(i in 1:n){d[i]=t(t(wine[i,1:11]-xbar))%*%invs%*%(t(wine[i,1:11]-xbar))}

plot(qchisq((seq(1:n)-0.5)/n,11),sort(d),pch=19,main="Chi-square plot",xlab="Chi-square quantiles",ylab="generalized distances",
      cex.axis=1.5, cex.lab=1.5,cex.main=1.5)
abline(0,1,lty =1 , cex = 1.5, col = 'red')
text(x = 28,y=233, 'Is Outlier -->')
#C
sort(d)
order(d)

t(t(wine[63,1:11]))
round(xbar,2)

#D only Ph satisfies normality, others could be transformed, proposed trans is boxcox with calculated lambda
source("functions.R")

#fixed 
boxcoxplot(wine$fixed_acidity)
boxcoxplot(wine$volatile_acidity)
boxcoxplot(wine$citric_acid)
boxcoxplot(wine$residual_sugar)
boxcoxplot(wine$chlorides)
boxcoxplot(wine$free_sulfur_dioxide)
boxcoxplot(wine$total_sulfur_dioxide)
boxcoxplot(wine$density)
boxcoxplot(wine$pH)
boxcoxplot(wine$sulphates)
boxcoxplot(wine$alcohol)


#E
#ANalysis needed

#Question 2
female <- read.table("T1-9.dat")
male <- read.table("T8-6.dat")

male = male[,1:6]
female = female[,1:6]
colnames(male) = c('Country', '100m', '200m', '400m', '800m', '1500m')
colnames(female) = c('Country', '100m', '200m', '400m', '800m', '1500m')

#A unequal variance
install.packages('ICSNP')
library(ICSNP)

HotellingsT2(male[,2:6],female[,2:6])

#B
xbar.male = apply(male[,2:6],2,mean)
xbar.female = apply(female[,2:6],2,mean)

s1 = cov(male[,2:6])
s2 = cov(female[,2:6])
n1 = nrow(male)
n2 = nrow(female)
p = 5

for(i in 1:5){
  lower = (xbar.male-xbar.female)[i] - sqrt(qchisq(0.95,p))*sqrt(s1[i,i]/n1+s2[i,i]/n2)
  upper = (xbar.male-xbar.female)[i] + sqrt(qchisq(0.95,p))*sqrt(s1[i,i]/n1+s2[i,i]/n2)
    print(c(lower,upper))
}

#C Population approach because it was more efficient