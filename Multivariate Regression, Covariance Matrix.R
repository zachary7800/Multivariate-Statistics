 track = read.table('T1-9.dat')

 colnames(track) = c('Country', 'x1_100m', 'x2_200m',
 'x3_400m', 'x4_800m', 'x5_1500m',
 'x6_3000m', 'x7_Marathon')

 #1a
 x_bar = apply(track[,2:8], 2, mean)
 x_bar

 var_x = var(track[,2:8])
 var_x

 cor_x = cor(track[,2:8])
 cor_x

 #1b
 v1 = (1/3)*track$x1_100m + (1/6)*track$x2_200m + (1/12)*track$x3_400m
 v2 = (1/2.4)*track$x4_800m + (1/4.5)*track$x5_1500m + (1/9)*track$x6_3000m

 # Dom Rep: V1: 12.28 V2: 2.98
 # Ireland: V1: 11.90 V2: 2.65
 # Guatemala: V1: 12.69 V2: 2.97
 # Denmark: V1: 12.11 V2: 2.73

 #1c
 mean(v1)
 mean(v2)
 var(v1)
 var(v2)
 cov(v1,v2)

 #1d
 x_b = apply(track[,2:4],2,mean)
 c = c(1/3,1/6,1/12)
 #t(c) %*% x_b

 x_b2 = apply(track[,5:7],2,mean)
 c2 = c(1/2.4,1/4.5,1/9)
 #t(c2) %*% x_b2

 v_bar = cbind(t(c) %*% x_b,t(c2) %*% x_b2)
 v_bar

 v1_s = t(c) %*% var_x[1:3,1:3] %*% c
 v2_s = t(c2) %*% var_x[4:6,4:6] %*% c2

 covmat = cbind(v1_s,v2_s)
 colnames(covmat)= c('v1','v2')

 covmatrix = t(c) %*% var_x[1:3,4:6] %*% (c2)

 s_matrix = cbind(rbind(v1_s,covmatrix),rbind(covmatrix,v2_s))
 s_matrix

#################################
 #Question 2#
 wine = as.data.frame(read.csv('wineFl2021.csv'))

 wine$type = as.factor(wine$type)


 lm.multi <- lm(alcohol ~ .^2, data = wine)
 summary(lm.multi)
 step(lm.multi)

 lmbest = (lm(formula = alcohol ~ fixed_acidity + volatile_acidity + citric_acid +
 residual_sugar + chlorides + free_sulfur_dioxide + total_sulfur_dioxide +
 density + pH + sulphates + quality + type + fixed_acidity:volatile_acidity +
 fixed_acidity:chlorides + fixed_acidity:free_sulfur_dioxide +
 fixed_acidity:density + fixed_acidity:quality + fixed_acidity:type +
 volatile_acidity:chlorides + volatile_acidity:density + volatile_acidity:type +
 citric_acid:residual_sugar + citric_acid:chlorides + citric_acid:type +
 residual_sugar:chlorides + residual_sugar:density + chlorides:free_sulfur_dioxide +
 chlorides:density + chlorides:quality + free_sulfur_dioxide:pH +
 free_sulfur_dioxide:sulphates + free_sulfur_dioxide:type +
 total_sulfur_dioxide:sulphates + total_sulfur_dioxide:type +
 density:pH + density:sulphates + density:type + sulphates:quality +
 quality:type, data = wine))

 summary(lmbest)
 plot(lmbest)

 #fixedacidity volatileacidity citricacid residulasugar chlorides freesulfdio totalsulfdio 
density pH Sulphates Quality
 # 8 0.6 0.6 15 0.3 50 200 
1 3.5 0.75 4
 x_new = data.frame(fixed_acidity = 8, volatile_acidity = 0.6, citric_acid = 0.6,
residual_sugar = 15,
 chlorides = 0.3, free_sulfur_dioxide = 50, total_sulfur_dioxide = 200,
density = 1,
 pH = 3.5, sulphates = 0.75, quality = 4, type = as.factor(1))
 predict.lm(lmbest, newdata = x_new, interval = 'predict', level = 0.95)

 #2.63 - 8.87

 ###########################################
 #Question 3#
 lm.multi2 <- lm(cbind(alcohol,quality) ~ ., data = wine)

 library("car")

 Manova(lm.multi2, type = "II", test =c('Wilks'))

 lmbest2 = lm(cbind(alcohol,quality)~(fixed_acidity + volatile_acidity + residual_sugar +
density + pH + sulphates +
 type) ^2, data = wine)

 Manova(lmbest2, type = "II", test = c('Wilks'))

 thebest = lm(cbind(alcohol,quality) ~ fixed_acidity + volatile_acidity + residual_sugar +
density + pH + sulphates
 + type + fixed_acidity:density + fixed_acidity:type +
residual_sugar:sulphates + density:sulphates +
 density:type + pH:sulphates, data = wine)
 #3a errors
 resid = thebest$residuals
 n = nrow(resid)
 hat.sigma = t(resid)%*%resid/n
 hat.sigma

 #3b 
 #Define our own "rstandard" method for "mlm" class
 rstandard.mlm <-function(model) {

 # Q matrix
 Q <-with(model,qr.qy(qr,diag(1, nrow =nrow(qr$qr), ncol = qr$rank)))
 # diagonal of hat matrix QQ'
 hii <-rowSums(Q^2)
 # residual sums of squares (for each model)
 RSS <-colSums(model$residuals^2)
 # Pearson estimate of residuals (for each model)
 sigma <-sqrt(RSS/model$df.residual)
 # point-wise residual standard error (for each model)
 pointwise_sd <-outer(sqrt(1-hii), sigma)
 # standardised residuals
 model$residuals/pointwise_sd
 }
 f <-fitted(thebest);
 r <-rstandard(thebest);
 par(mfcol=c(3,2))
 par(mar=c(4.5, 4, 1.5, 0.4)+0.1);
 #all variables together
 plot(f, r, col =as.numeric(col(f)), pch = 19, ylim =c(-3, 4))
 legend("topleft", legend =paste0("response ", 1:ncol(f)), pch = 19,col = 1:ncol(f),
text.col = 1:ncol(f))
 #or one at a time
 for(i in 1:ncol(f)){
 plot(f[,i],r[,i], main=paste("Response",i))
 qqnorm(r[,i], main=paste("Response",i))
 qqline(r[,i])
 }

 #3c 
 new_fixed = 8
 new_volatile = 0.6
 new_citric = 0.6
 new_residual = 15
 new_chlorides = 0.3
 new_free = 50
 new_total = 200
 new_density = 1
 new_pH = 3.5
 new_sulph = 0.75
 new_quality = 4
 new_type = 1

 x0 = c(1, new_fixed, new_volatile, new_residual, new_density, new_pH, new_sulph, new_type,
 new_pH*new_sulph, new_density*new_type, new_density*new_sulph,
new_residual*new_sulph, new_fixed*new_type,
 new_fixed*new_density)

 X = model.matrix(~ fixed_acidity + volatile_acidity + residual_sugar + density + pH +
sulphates + type +
 pH:sulphates + density:type + density:sulphates +
residual_sugar:sulphates +
 fixed_acidity:type + fixed_acidity:density, data = wine)

 m = 2
 r = 13
 hat.beta <- thebest$coeff
 hat.beta

 resid = thebest$resid
 n = nrow(resid)
 hat.sigma = t(resid)%*%resid/n
 hat.sigma

 multiplier = sqrt(qf(.95,m,n-r-m)*(m*(n-r-1))/(n-r-m))


 for(i in 1:m){
 mean.se <- sqrt(hat.sigma[i,i]*(1+x0%*%solve(t(X)%*%X)%*%x0)*(n/(n-r-1)))
 cat("Response",i,(x0%*%hat.beta)[i] - multiplier*mean.se, (x0%*%hat.beta)[i] +
multiplier*mean.se, "\n")
 }
x0 %*% hat.beta