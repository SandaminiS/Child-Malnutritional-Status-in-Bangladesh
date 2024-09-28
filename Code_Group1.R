##################################
### STA 6366 project code ########
#################################


df1 <- read.csv("C:\\Users\\sanda\\OneDrive - University of Central Florida\\UCF\\3_Fall_2023\\Courses\\STA6366 Statistical Methodology for Data Science I\\project\\datasceicne.csv")

#changing the class if variables
df1$overweight <- as.factor(df1$overweight)
df1$wasted <- as.factor(df1$wasted)
df1$stunted <- as.factor(df1$stunted)
df1$underweight <- as.factor(df1$underweight)

table(as.factor(df1$ANC))
df1$ANC <- ifelse(df1$ANC == "no", 0, 1)
df1$ANC <- as.factor(df1$ANC)

table(as.factor(df1$homedelivery))
df1$homedelivery <- ifelse(df1$homedelivery == "home", 1, 2)
df1$homedelivery <- as.factor(df1$homedelivery)

table(as.factor(df1$disability))
df1$disability <- ifelse(df1$disability == "no", 0, 1)
df1$disability <- as.factor(df1$disability)

table(as.factor(df1$cdisability))
df1$cdisability <- ifelse(df1$cdisability == "no", 0, 1)
df1$cdisability <- as.factor(df1$cdisability)


table(as.factor(df1$melevel))
df1$melevel <- ifelse(df1$melevel == " higher", 4, ifelse(df1$melevel == "secondary", 3, 
                                                          ifelse(df1$melevel == "primary", 2, 1)))
df1$melevel <- as.factor(df1$melevel)

table(as.factor(df1$illness))
df1$illness <- ifelse(df1$illness == "no", 0, 1)
df1$illness <- as.factor(df1$illness)

table(as.factor(df1$antibiotic))
df1$antibiotic <- ifelse(df1$antibiotic == "no", 0, 1)
df1$antibiotic <- as.factor(df1$antibiotic)

table(as.factor(df1$division))
df1$division <- ifelse(df1$division=="borishal", 1, 
                       ifelse(df1$division=="chitagong", 2, 
                              ifelse(df1$division=="comilla", 3, 
                                     ifelse(df1$division=="dhaka", 4, 
                                            ifelse(df1$division=="khulna", 5,
                                                   ifelse(df1$division=="maymenshing", 6, 
                                                          ifelse(df1$division=="rangpur", 7, 8)))))))
df1$division <- as.factor(df1$division)


table(as.factor(df1$sex))
df1$sex <- ifelse(df1$sex=="female", 0, 1)
df1$sex <- as.factor(df1$sex)


table(as.factor(df1$area))
df1$area <- ifelse(df1$area=="rural", 0, 1)
df1$area <- as.factor(df1$area)


table(as.factor(df1$helevel))
df1$helevel <- ifelse(df1$helevel == " higher", 4, ifelse(df1$helevel == "secondary", 3, 
                                                          ifelse(df1$helevel == "primary", 2, 1)))
df1$helevel <- as.factor(df1$helevel)


table(as.factor(df1$sanitation))
df1$sanitation <- ifelse(df1$sanitation=="flush", 1, 
                         ifelse(df1$sanitation=="notflush", 2, 3))
df1$sanitation <- as.factor(df1$sanitation)


table(as.factor(df1$pwater))
df1$pwater <- ifelse(df1$pwater=="tubewel", 1, 0)
df1$pwater <- as.factor(df1$pwater)


table(as.factor(df1$iodin))
df1$iodin <- ifelse(df1$iodin=="noiodin", 0, ifelse(df1$iodin=="0to15ppm",1,2))
df1$iodin <- as.factor(df1$iodin)


table(as.factor(df1$windex3))
df1$windex3 <- ifelse(df1$windex3=="poor", 1, ifelse(df1$windex3=="middle",2,3))
df1$windex3 <- as.factor(df1$windex3)


table(as.factor(df1$childbirthweight3))
df1$childbirthweight3 <- ifelse(df1$childbirthweight3=="verypoor", 1,
                                ifelse(df1$childbirthweight3=="average",2,3))
df1$childbirthweight3 <- as.factor(df1$childbirthweight3)

# deleting the variales with high missing values
df1<-df1[,-c(1,3,7,21)]

#omit the remaining na's
df.n <- na.omit(df1) 


#separating into datasets
underweight <- df.n[, 1:18]
stunted <- df.n[, c(1:17, 19)]
wasted <- df.n[, c(1:17, 20)]
overweight <- df.n[, c(1:17, 21)]


#under+over sampling
library(ROSE)
#underweight
data.balanced.uw <- ovun.sample(underweight ~., data=underweight,
                                N=nrow(underweight), p=0.5, 
                                seed=1, method="both")$data

underweight1 <- data.balanced.uw
table(data.balanced.uw$underweight)

#wasted
data.balanced.w <- ovun.sample(wasted ~., data=wasted,
                               N=nrow(wasted), p=0.5, 
                               seed=1, method="both")$data
wasted1 <- data.balanced.w
table(data.balanced.w$wasted)

#stunted
data.balanced.s <- ovun.sample(stunted ~., data=stunted,
                               N=nrow(stunted), p=0.5, 
                               seed=1, method="both")$data
stunted1 <- data.balanced.s
table(data.balanced.s$stunted)

#overwight
data.balanced.ow <- ovun.sample(overweight ~., data=overweight,
                                N=nrow(overweight), p=0.5, 
                                seed=1, method="both")$data
overweight1 <- data.balanced.ow
table(data.balanced.ow$overweight)

# from this point onwards
# same application is done for each of 4 malnutrition instances.
# however, we are only proving 1 code instance

###### Bivariate analysis ####

#####  1 ##### 

chage_var <- var.test(underweight1$chage~underweight1$underweight)      
chage_result<-t.test(underweight1$chage~underweight1$underweight,alternative = "two.sided",var.equal = FALSE) 
#The variance of two groups are  not equal
#There is difference between the two groups(Means are not equal)


##### 2 #####

wageatb_var<-var.test(underweight1$wageatb~underweight1$underweight) 
wageatb_result<-t.test(underweight1$wageatb~underweight1$underweight,alternative = "two.sided",var.equal = FALSE) 
#The variance of two groups are  not equal
#There is difference between the two groups(Means are not equal)


##### 3 #####
homedelivery_result <-chisq.test(underweight1$underweight, underweight1$homedelivery)
# There is relationship, (The two categorical variables are dependent)


##### 4 #####
disability_result <-chisq.test(underweight1$underweight, underweight1$disability)
# There is no relationship, (The two categorical variables are independent)


##### 5 #####
melevel_result <-chisq.test(underweight1$underweight, underweight1$melevel)
# There is relationship, (The two categorical variables are dependent)


##### 6 #####
illness_result <-chisq.test(underweight1$underweight, underweight1$illness)
# There is relationship, (The two categorical variables are dependent)


##### 7 #####
antibiotic_result <-chisq.test(underweight1$underweight, underweight1$antibiotic)
# There is no relationship, (The two categorical variables are independent)

##### 8 #####
division_result <-chisq.test(underweight1$underweight, underweight1$division)
# There is relationship, (The two categorical variables are dependent)


##### 9 #####
sex_result <-chisq.test(underweight1$underweight, underweight1$sex)
# There is relationship, (The two categorical variables are dependent)

##### 10 #####
area_result <-chisq.test(underweight1$underweight, underweight1$area)
# There is relationship, (The two categorical variables are dependent)


##### 11 #####
hhsize_var <- var.test(underweight1$hhsize~underweight1$underweight)      
hhsize_result<-t.test(underweight1$hhsize~underweight1$underweight,alternative = "two.sided",var.equal = FALSE) 
#The variance of two groups are  not equal
#There is difference between the two groups(Means are not equal)



##### 12 #####
helevel_result <-chisq.test(underweight1$underweight, underweight1$helevel)
# There is relationship, (The two categorical variables are dependent)

##### 13 #####
sanitation_result <-chisq.test(underweight1$underweight, underweight1$sanitation)
# There is relationship, (The two categorical variables are dependent)

##### 13 #####
pwater_result <-chisq.test(underweight1$underweight, underweight1$pwater)
# There is relationship, (The two categorical variables are dependent)


##### 14 #####
iodin_result <-chisq.test(underweight1$underweight, underweight1$iodin)
# There is relationship, (The two categorical variables are dependent)

##### 15 #####
iodin_result <-chisq.test(underweight1$underweight, underweight1$iodin)
# There is relationship, (The two categorical variables are dependent)

##### 16 #####
nochildbirth_var<-var.test(underweight1$nochildbirth~underweight1$underweight)  
nochildbirth_result<-t.test(underweight1$nochildbirth~underweight1$underweight,alternative = "two.sided",var.equal = FALSE) 
#The variance of two groups are  not equal
#There is difference between the two groups(Means are not equal)


##### 17 #####
windex3_result <-chisq.test(underweight1$underweight, underweight1$windex3)
# There is relationship, (The two categorical variables are dependent)




library("FactoMineR")
library("factoextra")
library(vcd)

library(dplyr)


########################################################################
################# Logistic with FAMD ##################################
#######################################################################


underdata=factordata[,-c(18,19,20)]
undersample <- ovun.sample(overweight ~., data=underdata,
                           N=nrow(underdata), p=0.5, 
                           seed=1, method="both")$data

#View(undersample)
#table(undersample$underweight)
set.seed(111)
ind <- sample(2, nrow(undersample),
              replace = TRUE,
              prob = c(0.80, 0.2))
train <- undersample[ind==1,]
#View(training)
test <- undersample[ind==2,]
table(train$overweight)

mca_train<- FAMD(train[,-18],ncp=10, graph=FALSE)

mca_features <- as.data.frame(mca_train$ind$contrib)


#mca_features=scale(round(mca_features*1000,3))

mca_data<- data.frame(cbind(mca_features, overweight = train$overweight))


logit_underweight <- glm(overweight ~., data = mca_data, family = binomial(link = 'logit'))
#summary(logit_underweight)
mca_test<- FAMD(test[,-c(18,19,20,21)], ncp=10,graph=FALSE)

mca_testfeature <- as.data.frame(mca_test$ind$contrib)


# Combine MCA features with the outcome variable
test_mca_data <- data.frame(cbind(Intercept = 1, mca_testfeature))

predictions <- predict(logit_underweight, newdata = test_mca_data, type = "response")

prediction=ifelse(predictions>0.50,1,0)
#length(prediction)
table(prediction)

mca_test <- cbind(mca_testfeature, overweight = test$overweight)

conf_underweight=table(mca_test$overweight,prediction)
accur_underweight <- sum(diag(conf_underweight)) / sum(conf_underweight)
accur_underweight
speci_und=conf_underweight[1,1]/(conf_underweight[1,1]+conf_underweight[1,2])
sensi_und=conf_underweight[2,2]/(conf_underweight[2,1]+conf_underweight[2,2])
overweight=cbind(sensi_und,speci_und,accur_underweight)
goodness=cbind(var=c('underweight','staunted','wasted','overweight'),rbind(underweight,stunted,wasted,overweight))


########################################################################
############################## LASSO ##################################
#######################################################################



overdata=factordata[,-c(18,19,20)]
undersample <- ovun.sample(overweight ~., data=overdata,
                           N=nrow(overdata), p=0.5, 
                           seed=1, method="both")$data

#View(undersample)
#table(undersample$underweight)
set.seed(111)
ind <- sample(2, nrow(undersample),
              replace = TRUE,
              prob = c(0.80, 0.2))
train <- undersample[ind==1,]
#View(training)
test <- undersample[ind==2,]


library(glmnet)
train=na.omit(train)
test=na.omit(test)
dim(test)
# Creating a binary response variable
train_overweight <- train$overweight
#length(train_underweight)
# Creating a matrix of predictor variables
train_predictors <- data.matrix(train[,-18])
dim(train_predictors)

testdata=data.matrix(test[,-18])
dim(testdata)
test_overweight=test$overweight
# Fit Lasso logistic regression on the training data
lasso_overweight <- cv.glmnet(train_predictors, train_overweight, family = binomial(link='logit'), alpha = 1)


# Make predictions on the test set
predictions <- predict(lasso_overweight, newx = testdata, type = "response")
length(predictions)
# Convert predicted probabilities to binary predictions (0 or 1)
binary_predictions <- ifelse(predictions > 0.5, 1, 0)
length(binary_predictions)
undertable=table(test_overweight,binary_predictions)

sensunder=undertable[2,2]/(undertable[2,1]+undertable[2,2])
specunder=undertable[1,1]/(undertable[1,1]+undertable[1,2])
accuunder=sum(diag(undertable))/sum(undertable)

overweightgoodness=cbind(sensunder,specunder,accuunder)

cbind(var=c('underweight', 'stunted','wasted','overweight'),rbind(undergoodness,stuntedgoodness,wastedgoodness,overweightgoodness))


########################################################################
################# Random Forest ########################################
#######################################################################


ind <- sample(2, nrow(overweight1), replace = TRUE, prob = c(0.7, 0.3))
under_train<- overweight1[ind==1,]
under_test<- overweight1[ind==2,]

str(under_test)

rf_overweight <- randomForest(overweight~., data=under_train, proximity=TRUE) 

pred <- predict(rf_overweight, under_test[-22])
table(pred, under_test$overweight)


########################################################################
################# SVM #################################################
#######################################################################

underdata=factordata[,-c(18,19,20)]
undersample <- ovun.sample(overweight ~., data=underdata,
                           N=nrow(underdata), p=0.5, 
                           seed=1, method="both")$data

set.seed(111)
ind <- sample(2, nrow(undersample),
              replace = TRUE,
              prob = c(0.80, 0.2))
train <- undersample[ind==1,]
#View(training)
test <- undersample[ind==2,]

SVM_train=na.omit(train)
dim(SVM_train)
#SVM_data<- cbind(mca_features, stunted = train$stunted)
#View(mca_data)
SVM_train$overweight = factor(SVM_train$overweight, levels = c(0, 1)) 


SVM_test=na.omit(test)
dim(SVM_test)
test_overweight=as.factor(SVM_test$overweight)
#table(SVM_test$stunted)
SVM_test<- test[,-18]
#dim(SVM_test)

#table(test_underweight)
# Fitting SVM to the Training set 
#install.packages('e1071') 
library(e1071) 

SVMclassifier = svm(overweight ~ ., 
                    data = SVM_train, 
                    type = 'C-classification', 
                    kernel = 'linear') 

y_pred = predict(SVMclassifier, newdata = SVM_test) 

conunderweight=table(test_overweight,y_pred)

#confusionMatrix(test_underweight,y_pred)
sen=conunderweight[2,2]/(conunderweight[2,2]+conunderweight[2,1])
spec=conunderweight[1,1]/(conunderweight[1,1]+conunderweight[1,2])
accu=sum(diag(conunderweight))/sum(conunderweight)
goodsoverweight=cbind(sen,spec,accu)

cbind(var=c('underweight','stunted','wasted','overweight'),rbind(goodunderweight,goodstunted,goodswasted,goodsoverweight))


