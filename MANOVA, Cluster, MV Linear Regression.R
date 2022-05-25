setwd("~/730")
cpu <- read.csv("machine (1) (1).data",header = FALSE)
colnames(cpu) <- c('Vendor Name','Model Name','Machine Cycle Time', 'Minimum Main Memory', 'Maximum Main Memory',
                   'Cache Memory', 'Minimum Channels', 'Maximum Channels', 'Published Relative Performance', 
                   'Estimated Relative Performance')

hist(cpu$`Machine Cycle Time`)
hist(cpu$`Minimum Main Memory`)
hist(cpu$`Maximum Main Memory`)
hist(cpu$`Cache Memory`)
hist(cpu$`Minimum Channels`)
hist(cpu$`Maximum Channels`)
hist(cpu$`Published Relative Performance`)

x_bar = apply(cpu[,3:9],2,mean)
round(x_bar,2)
cov(cpu[,3:9])
s_bar = cor(cpu[,3:9])
round(s_bar,2)

# Ward Hierarchical Clustering
d <- dist(t(cpu[,3:9]), method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=3, border="red")

#Average Hierarchical Clustering
ad <- dist(t(cpu[,3:9]), method = "euclidean") # distance matrix
afit <- hclust(ad, method="average")
plot(afit) # display dendogram
agroups <- cutree(afit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(afit, k=3, border="red")

clust2 <-kmeans(t(cpu[,3:9]), 2)
clust3 <-kmeans(t(cpu[,3:9]), 3)
clust4 <-kmeans(t(cpu[,3:9]), 4)
clust5 <-kmeans(t(cpu[,3:9]), 5)
clust2
clust3 
clust4 
clust5 

temp2 = cpu$`Published Relative Performance`
cpu = cpu[,3:8]
lmbiv = lm(temp2 ~.^2,data = cpu)
ncol(cpu)
summary(lmbiv)
step(lmbiv)

lmbest = lm(formula = temp2 ~ `Machine Cycle Time` + `Minimum Main Memory` + 
              `Maximum Main Memory` + `Cache Memory` + `Minimum Channels` + 
              `Maximum Channels` + `Machine Cycle Time`:`Cache Memory` + 
              `Machine Cycle Time`:`Maximum Channels` + `Minimum Main Memory`:`Maximum Main Memory` + 
              `Minimum Main Memory`:`Cache Memory` + `Minimum Main Memory`:`Maximum Channels` + 
              `Maximum Main Memory`:`Cache Memory` + `Maximum Main Memory`:`Minimum Channels` + 
              `Maximum Main Memory`:`Maximum Channels` + `Cache Memory`:`Minimum Channels` + 
              `Cache Memory`:`Maximum Channels` + `Minimum Channels`:`Maximum Channels`, 
            data = cpu)
summary(lmbest)
plot(lmbest)


# test for univariate normality of the two variables
##for first group
apply(morel[morel$studentgroup == 1, -1], 2, shapiro.test)

##for second group
apply(morel[morel$studentgroup == 2, -1], 2, shapiro.test)

##for third group
apply(morel[morel$studentgroup == 3, -1], 2, shapiro.test)


#Use the manova function

Y <- as.matrix(cpu[ ,3:9])
Group <- cpu$`Vendor Name`  
fit <- manova(Y ~ Group)

#  Print ANOVA tales for each response
summary.aov(fit)

#  Print MANOVA test statistics
summary.manova(fit,test = c("Wilks"))
summary.manova(fit,test = c("Pillai"))
summary.manova(fit,test = c("Hotelling-Lawley"))
summary.manova(fit,test = c("Roy"))

#  Print MANOVA Table
Lambda<-summary.manova(fit,test = c("Wilks"))
Lambda$SS
################################################################################