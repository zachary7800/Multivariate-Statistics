 school = read.csv('APBI_NYcounty.csv')
 school

 install.packages('tidyverse')
 library(tidyverse)


 apbi_student = filter(school, Values == 'student_cnt')
 apbi_prof = filter(school, Values == 'proficient_student_cnt')

 #2C
 clust2 <-kmeans(apbi_student[,4:14], 2)
 clust3 <-kmeans(apbi_student[,4:14], 3)
 clust5 <-kmeans(apbi_student[,4:14], 5)
 clust7 <-kmeans(apbi_student[,4:14], 7)
 clust10 <-kmeans(apbi_student[,4:14], 10)

 library(cluster)
 clusplot(apbi_student[,4:14], clust2$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=2')
 clusplot(apbi_student[,4:14], clust3$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=3')
 clusplot(apbi_student[,4:14], clust5$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=5')
 clusplot(apbi_student[,4:14], clust7$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=7')
 clusplot(apbi_student[,4:14], clust10$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=10')

 #2d
 aclust2 <-kmeans(scale(apbi_student[,4:14]), 2)
 aclust3 <-kmeans(scale(apbi_student[,4:14]), 3)
 aclust5 <-kmeans(scale(apbi_student[,4:14]), 5)
 aclust7 <-kmeans(scale(apbi_student[,4:14]), 7)
 aclust10 <-kmeans(scale(apbi_student[,4:14]), 10)

 clusplot(scale(apbi_student[,4:14]), aclust2$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=2')
 clusplot(scale(apbi_student[,4:14]), aclust3$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=3')
 clusplot(scale(apbi_student[,4:14]), aclust5$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=5')
 clusplot(scale(apbi_student[,4:14]), aclust7$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=7')
 clusplot(scale(apbi_student[,4:14]), aclust10$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=10')

 #2ec
 pclust2 <-kmeans(apbi_prof[,4:14], 2)
 pclust3 <-kmeans(apbi_prof[,4:14], 3)
 pclust5 <-kmeans(apbi_prof[,4:14], 5)
 pclust7 <-kmeans(apbi_prof[,4:14], 7)
 pclust10 <-kmeans(apbi_prof[,4:14], 10)


 clusplot(apbi_prof[,4:14], clust2$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=2')
 clusplot(apbi_prof[,4:14], clust3$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=3')
 clusplot(apbi_prof[,4:14], clust5$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=5')
 clusplot(apbi_prof[,4:14], clust7$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=7')
 clusplot(apbi_prof[,4:14], clust10$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=10')

 #2de
 saclust2 <-kmeans(scale(apbi_prof[,4:14]), 2)
 saclust3 <-kmeans(scale(apbi_prof[,4:14]), 3)
 saclust5 <-kmeans(scale(apbi_prof[,4:14]), 5)
 saclust7 <-kmeans(scale(apbi_prof[,4:14]), 7)
 saclust10 <-kmeans(scale(apbi_prof[,4:14]), 10)

 clusplot(scale(apbi_prof[,4:14]), saclust2$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=2')
 clusplot(scale(apbi_prof[,4:14]), saclust3$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=3')
 clusplot(scale(apbi_prof[,4:14]), saclust5$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=5')
 clusplot(scale(apbi_prof[,4:14]), saclust7$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=7')
 clusplot(scale(apbi_prof[,4:14]), saclust10$cluster, color=TRUE, shade=TRUE,
 labels=2, lines=0, main='k=10')