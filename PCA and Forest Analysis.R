wine <- read.csv("wineFl2021.csv")

#a
cov <- cov(wine[,1:11])
cov

cor <- cor(wine[,1:11])
round(cor,2)

#b COR
pca.s <- prcomp(wine[,1:11], center=TRUE, scale=TRUE)
pca.s
screeplot(pca.s,type="lines")


#c 3 
summary(pca.s)
pca.s$sdev^2

#d 
corr <- cor(wine[,1:11], pca.s$x[,1:3])
round(corr, 2)

#e
## PC 1 = sulfur dioxides
## PC 2 = Dense Alcohol Sugar
## PC 3 = PH Citric Acid

#f
#No because there is not a single component that highly correlates with all three principal components.

#g
plot(pca.s$x[,1],pca.s$x[,2])
#wine 0 is on the left and wine 1 is on the right
#there is one outlier

#Bonus point
qqnorm(pca.s$x[,1])
qqline(pca.s$x[,1])
##The pca does not follow a normal distribution

##############################################################
#A
cor2 <- cor(wine[,1:12])
round(cor2,2)

cov2 <- cov(wine[,1:12])
cov2

#B
pca.r <- prcomp(wine[,1:13], center=TRUE, scale=TRUE)
pca.r
screeplot(pca.r,type="lines")

#c 4
summary(pca.r)
pca.r$sdev^2

#d
corr2 <- cor(wine[,1:13], pca.r$x[,1:4])
round(corr2, 2)

#e
## PC 1 = Sulfur Dioxide Type
## PC 2 = Dense Alcohol
## PC 3 = Citric Acid
## PC 4 = SUlphates
## The new interpretations slightly differ but have similarities

#f
#No there is not a single component that is highly correlated with each principle component

#g
plot(pca.r$x[,1],pca.r$x[,2])
#wine 0 is on the left and wine 1 is on the right
#one outlier
#The findings were the same on part 1 and 2

##Bonus Points
#Without because it gives us less dimensions 
#cluster analysis


######################################################
#3
#A
library(rpart)
library(rpart.plot)
fit <- rpart(wine$quality ~.,
             method="class", data=wine)

plotcp(fit) # visualize cross-validation results 
# prune the tree 
pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Wine Quality")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
prp(pfit,main = "Pruned Classification Tree for Wine Quality")

#b
#Quality == 6 

#c
install.packages("randomForest")
library(randomForest)
wine$type = as.factor(wine$type)
wine$quality = as.factor(wine$quality)

fit2 <- randomForest(quality ~., 
                    data=wine, proximity = T, importance = T)



print(fit2) # view results 
plot(fit2)

fit3 <- randomForest(quality ~.,proximity = T, importance = T, 
                     data=wine,ntree = 1, mtry = 1, nodesize = 1)
fit4 <- randomForest(quality ~.,proximity = T, importance = T, 
                     data=wine,ntree = 3, mtry = 2, nodesize = 2)
fit5 <- randomForest(quality ~.,proximity = T, importance = T, 
                     data=wine,ntree = 5, mtry = 3, nodesize = 4)
fit6 <- randomForest(quality ~.,proximity = T, importance = T, 
                     data=wine,ntree = 7, mtry = 5, nodesize = 5)
fit7 <- randomForest(quality ~.,proximity = T, importance = T, 
                     data=wine,ntree = 3, mtry = 8, nodesize = 9)

##the print 
print(fit6)
plot(fit6)
wine.mds <- cmdscale(1 - fit6$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(wine[,1:11], wine.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(wine$quality)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(wine.mds$GOF)


##proximity plot
plot(wine.mds$points,col=c(1:5)[as.numeric(wine$quality)])

#d
# predict Just like a lm()
x.new <- data.frame(fixed_acidity = 8,volatile_acidity = 0.6, citric_acid = 0.6, residual_sugar = 15, 
                    chlorides = 0.3, free_sulfur_dioxide = 50, total_sulfur_dioxide = 200, density = 1,
                    pH = 3.5, sulphates = 0.75, alcohol = 10, type = 1)

wine0 <- read.csv("winefl2021.csv")
wine0$quality = as.factor(wine$quality)
fit60 <- randomForest(quality ~.,proximity = T, importance = T, 
                     data=wine0,ntree = 7, mtry = 5, nodesize = 5)
# the Out-of-bag prediction
predict(fit60)
# result from Random Forest
predict(fit60,x.new, type = "response")
# you want to see probabilities 
predict(fit60,x.new, type="prob")

#e i got the same results when comparing the fitted tree to the random forest