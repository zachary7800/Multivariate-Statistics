setwd("~/730")
body <- read.table("bodydata.txt")
colnames(body) = c("Biacromial Diameter","Biiliac diameter","Bitrochanteric diameter",
                   "Chest depth","Chest diameter","Elbow diameter","Wrist diameter",
                   "Knee diameter","Ankle diameter","Shoulder girth","Chest girth",
                   "Waist girth","Navel girth","Hip girth","Thigh girth","Bicep girth",
                   "Forearm girth","Knee girth","Calf girth","Ankle girth","Wrist girth",
                   "Age","Weight","Height","Gender")
#1
BMI = body$Weight / ((body$Height/100)^2)
body = cbind(body,BMI)

summary(skel$BMI)
#2
set.seed(20135)

skel = body[sample(nrow(body),300),]

#3 Need an exploratory data analysis
hist(skel$`Biacromial Diameter`)
hist(skel$`Biiliac diameter`)
hist(skel$`Chest depth`)
hist(skel$`Chest diameter`)
hist(skel$`Elbow diameter`)
hist(skel$`Wrist diameter`)
hist(skel$`Knee diameter`)
hist(skel$`Ankle diameter`)
hist(skel$`Shoulder girth`)
hist(skel$`Chest girth`)
hist(skel$`Waist girth`)
hist(skel$`Navel girth`)
hist(skel$`Hip girth`)
hist(skel$`Thigh girth`)
hist(skel$`Bicep girth`)
hist(skel$`Forearm girth`)
hist(skel$`Knee girth`)
hist(skel$`Calf girth`)
hist(skel$`Ankle girth`)
hist(skel$`Wrist girth`)
hist(skel$Age)
hist(skel$Weight)
hist(skel$Height)
hist(skel$Gender)
hist(skel$`Bitrochanteric diameter`)

boxplot(skel$`Biacromial Diameter`, main = "Biacromial Diameter")
boxplot(skel$`Biiliac diameter`,main = "Biiliac Diameter")
boxplot(skel$`Chest depth`,main = "Chest Depth")
boxplot(skel$`Chest diameter`,main = "Chest Diameter")
boxplot(skel$`Elbow diameter`,main = "Elbow Diameter")
boxplot(skel$`Wrist diameter`,main = "Wrist Diameter")
boxplot(skel$`Knee diameter`,main = "Knee Diameter")
boxplot(skel$`Ankle diameter`,main = "Ankle Diameter")
boxplot(skel$`Shoulder girth`,main = "Shoulder girth")
boxplot(skel$`Chest girth`,main = "Chest girth")
boxplot(skel$`Waist girth`,main = "Waist girth")
boxplot(skel$`Navel girth`,main = "Navel girth")
boxplot(skel$`Hip girth`,main = "Hip girth")
boxplot(skel$`Thigh girth`,main = "Thigh girth")
boxplot(skel$`Bicep girth`,main = "Bicep girth")
boxplot(skel$`Forearm girth`,main = "Forearm girth")
boxplot(skel$`Knee girth`,main = "Knee girth")
boxplot(skel$`Calf girth`,main = "Calf girth")
boxplot(skel$`Ankle girth`,main = "Ankle girth")
boxplot(skel$`Wrist girth`,main = "Wristr girth")
boxplot(skel$Age,main = "Age")
boxplot(skel$Weight,main = "Weight")
boxplot(skel$Height,main = "Height")
boxplot(skel$Gender,main = "Gender")
boxplot(skel$`Bitrochanteric diameter`,main = "Bitrochanteric Diameter")

x_bar = apply(skel[,1:26],2,mean)
x_bar

var_x = var(skel[,1:26])
var_x

cor_x = cor(skel[,1:26])
round(cor_x,2)
diag(cor_x)
#4
# Ward Hierarchical Clustering
d <- dist(t(skel[,1:21]), method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")

#Average Hierarchical Clustering
ad <- dist(t(skel[,1:21]), method = "euclidean") # distance matrix
afit <- hclust(ad, method="average")
plot(afit) # display dendogram
agroups <- cutree(afit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(afit, k=5, border="red")

clust3 <-kmeans(t(skel[,1:21]), 3)
clust4 <-kmeans(t(skel[,1:21]), 4)
clust5 <-kmeans(t(skel[,1:21]), 5)
clust6 <-kmeans(t(skel[,1:21]), 6)
clust7 <-kmeans(t(skel[,1:21]), 7)
clust8 <-kmeans(t(skel[,1:21]), 8)
clust3 #92.3%
clust4 #95.4%
clust5 #95.9%
clust6 #97.8%
clust7 #96.5%
clust8 #98.5%

#5
WeightStatus = ""
skel$WeightStatus = WeightStatus
for(i in 1:nrow(skel)){
  if(skel$BMI[i] < 18.5){
    skel$WeightStatus[i] = "Underweight"
  } else if(skel$BMI[i] < 24.9){
    skel$WeightStatus[i] = "Normal Weight"
  } else if(skel$BMI[i] < 29.9){
    skel$WeightStatus[i] = "Overweight"
  } else{
    skel$WeightStatus[i] = "Obese"
  }
}
#Discrim and Class Analys
### Estimated E(AER)
# lda type equal priors Hold-out
fit.cv <- lda(skel$WeightStatus ~ skel$`Biacromial Diameter` + skel$Height + skel$Gender + skel$`Biiliac diameter` +
                skel$`Chest depth`+ skel$`Chest diameter`+skel$`Elbow diameter`+skel$`Wrist diameter`+skel$`Knee diameter` +
                skel$`Ankle diameter`+skel$`Shoulder girth`+skel$`Chest girth`+skel$`Waist girth`+skel$`Navel girth`+
                skel$`Hip girth`+skel$`Thigh girth`+skel$`Bicep girth`+skel$`Forearm girth`+skel$`Knee girth`+skel$`Calf girth`+
                skel$`Ankle girth`+skel$`Wrist girth`+skel$Age+skel$Height+skel$Gender+skel$`Bitrochanteric diameter`,
              data= skel, CV=TRUE)
table(skel$WeightStatus,fit.cv$class)

library(MASS)
library(dplyr)
library(ggplot2)
model <- lda(skel$WeightStatus~., data = skel)
model
predictions <- model %>% predict(skel)
names(predictions)

lda.data <- cbind(skel, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = skel$WeightStatus))
mean(predictions$class==skel$WeightStatus)

#Cluster
temp = skel
temp = temp[,-23]
aclust3 <-kmeans(t(temp[,1:25]), 3)
aclust4 <-kmeans(t(temp[,1:25]), 4)
aclust5 <-kmeans(t(temp[,1:25]), 5)
aclust6 <-kmeans(t(temp[,1:25]), 6)
aclust7 <-kmeans(t(temp[,1:25]), 7)
aclust8 <-kmeans(t(temp[,1:25]), 8)
aclust3 #90.6%
aclust4 #82.5%
aclust5 #96.4%
aclust6 #96.9%
aclust7 #98.3%
aclust8 #98.6%


#6
skel = skel[,1:26]
lmbiv = lm(cbind(skel$Weight,skel$BMI) ~.,data = skel)
ncol(skel)
summary(lmbiv)
library("car")
Manova(lmbiv, type = "II", test = c('Wilks'))
lmbest = lm(cbind(skel$Weight,skel$BMI)~(skel$`Chest depth`+skel$`Chest diameter`+
                                           skel$`Knee diameter`+ skel$`Chest girth`+skel$`Waist girth`+
                                           skel$`Navel girth`+skel$`Hip girth`+skel$`Thigh girth`+skel$`Calf girth`+
                                           skel$Height+skel$Gender)^2,data = skel)
Manova(lmbest, ttpe = "II", test = c('Wilks'))
best = lm(cbind(skel$Weight,skel$BMI)~(Chest Diameter+ Knee Diameter+ Chest Girth+ Waist Girth+ Hip Girth
                                       + Thigh Girth+Calf Girth+Height+Chest girth:Height
                                       +waist girth:Height+thigh girth:Height+
                                         calf girth:Height + calf girth:Gender + height:Gender))
