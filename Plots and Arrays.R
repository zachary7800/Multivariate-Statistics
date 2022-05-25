APBI <- read.csv("APBI_NYcounty.csv", stringsAsFactors = TRUE)
APBI_cnt <- APBI[ APBI$Values=="student_cnt",]
APBI_prof <- APBI[ APBI$Values=="proficient_student_cnt",]


##Question b
hist(APBI_cnt$BusinessMarketing)
hist(APBI_cnt$ComputerSciences)
hist(APBI_cnt$EnglishLanguage)
hist(APBI_cnt$FinePerformingArts)
hist(APBI_cnt$GlobalStudies)
hist(APBI_cnt$Mathematics)
hist(APBI_cnt$Other)
hist(APBI_cnt$ReligiousEducation)
hist(APBI_cnt$Science)
hist(APBI_cnt$SecondLanguages)
hist(APBI_cnt$SocialStudies)


##Question c
boxplot(APBI_cnt$BusinessMarketing, main="Business Marketing")
boxplot(APBI_cnt$ComputerSciences, main="Computer Science")
boxplot(APBI_cnt$EnglishLanguage, main="English Language")
boxplot(APBI_cnt$FinePerformingArts, main="Fine Performing Arts")
boxplot(APBI_cnt$GlobalStudies, main="Global Studies")
boxplot(APBI_cnt$Mathematics, main="Mathematics")
boxplot(APBI_cnt$Other, main="Other")
boxplot(APBI_cnt$ReligiousEducation, main="Religious Education")
boxplot(APBI_cnt$Science, main="Science")
boxplot(APBI_cnt$SecondLanguages, main="Second Languages")
boxplot(APBI_cnt$SocialStudies, main="Social Studies")

##Question d
install.packages("car")
library(car)
spm(APBI_cnt[,4:14], var.labels=c("Business Marketing","Computer Science","English Language","Fine Performing Arts",
                           "Global Studies","Mathematics","Other","Religious Education","Science","Second Language",
                           "Social Studies"), diagonal=list(method="boxplot"),smooth=F,regLine=F,cex.axis=1.5,
    cex.labels=1.5,pch=20)

##Question e
x_bar = apply(APBI_cnt[,4:14], 2, mean)
x_bar

var_x = var(APBI_cnt[,4:14])
var_x

cor_x = cor(APBI_cnt[,4:14])
cor_x
diag(cor_x)

##Question f
##Did on Excel

##Question g
hist(APBI_cnt$BusinessMarketing[APBI_cnt$BusinessMarketing>0])
hist(APBI_cnt$ComputerSciences[APBI_cnt$ComputerSciences>0])
hist(APBI_cnt$EnglishLanguage[APBI_cnt$EnglishLanguage>0])
hist(APBI_cnt$FinePerformingArts[APBI_cnt$FinePerformingArts>0])
hist(APBI_cnt$GlobalStudies[APBI_cnt$GlobalStudies>0])
hist(APBI_cnt$Mathematics[APBI_cnt$Mathematics>0])
hist(APBI_cnt$Other[APBI_cnt$Other>0])
hist(APBI_cnt$ReligiousEducation[APBI_cnt$ReligiousEducation>0])
hist(APBI_cnt$Science[APBI_cnt$Science>0])
hist(APBI_cnt$SecondLanguages[APBI_cnt$SecondLanguages>0])
hist(APBI_cnt$SocialStudies[APBI_cnt$SocialStudies>0])

bm = sum(APBI_cnt$BusinessMarketing[APBI_cnt$BusinessMarketing>0])/sum(APBI_cnt$BusinessMarketing>0)

cs = sum(APBI_cnt$ComputerSciences[APBI_cnt$ComputerSciences>0])/sum(APBI_cnt$ComputerSciences>0)

el = sum(APBI_cnt$EnglishLanguage[APBI_cnt$EnglishLanguage>0])/sum(APBI_cnt$EnglishLanguage>0)

fpa = sum(APBI_cnt$FinePerformingArts[APBI_cnt$FinePerformingArts>0])/sum(APBI_cnt$FinePerformingArts>0)

gs = sum(APBI_cnt$GlobalStudies[APBI_cnt$GlobalStudies>0])/sum(APBI_cnt$GlobalStudies>0)

m = sum(APBI_cnt$Mathematics[APBI_cnt$Mathematics>0])/sum(APBI_cnt$Mathematics>0)

o = sum(APBI_cnt$Other[APBI_cnt$Other>0])/sum(APBI_cnt$Other>0)

re = sum(APBI_cnt$ReligiousEducation[APBI_cnt$ReligiousEducation>0])/sum(APBI_cnt$ReligiousEducation>0)

s = sum(APBI_cnt$Science[APBI_cnt$Science>0])/sum(APBI_cnt$Science>0)

sl = sum(APBI_cnt$SecondLanguages[APBI_cnt$SecondLanguages>0])/sum(APBI_cnt$SecondLanguages>0)

ss = sum(APBI_cnt$SocialStudies[APBI_cnt$SocialStudies>0])/sum(APBI_cnt$SocialStudies>0)

xbargraph = cbind(bm,cs,el,fpa,gs,m,o,re,s,sl,ss)
xbargraph
