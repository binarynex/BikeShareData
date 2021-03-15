###-------------------------------------------------------------------
### Group EPSILON:
### Nicole Castonguay, Ryan Goebel, Tim Przybylowicz, Brad Viles
###
### ALY6015.23546
### Final Project
###-------------------------------------------------------------------

###----------------------------------------------------------------------------
### Dependency Installation/Loading & Saving Existing Graphical Parameters
###----------------------------------------------------------------------------

# Install and load required packages

install.packages("corrplot")    #used for creating correlograms
install.packages("glmnet")      #glmnet used to implement LASSO
install.packages("dplyr")   # for dataset wrangling
install.packages("gginference")  # for hypothesis test plots
install.packages("randomForest") # used for random forest analysis
install.packages("vip") # used for variable importance plots
install.packages("ggplot2") # used for visuals
library("corrplot")
library("glmnet")
library("gginference")
library("dplyr")
library("randomForest")
library("vip")
library("ggplot2")

# Save old graphics parameters prior to start
opar <- par(no.readonly = TRUE)   #Define Original Parameters

###----------------------------------------------------------------------------
### Data Input & Clean-up
### ---------------------------------------------------------------------------

# Bikedataset.csv located at: https://archive.ics.uci.edu/ml/machine-learning-databases/00560/
# Open "SeoulBikeData.csv" dataset
path <- file.choose()  # Set file path on local computer
bikes <- read.csv(path, stringsAsFactors = FALSE, # import the data
                          header=TRUE, check.names = TRUE  # check and adjust special characters in header
                          ) 

# Examine initial dataset structure
str(bikes)

# Simplify column/variable names
colnames(bikes) <- c("calendar", "count", "hour", "temp", "humidity", "wind", 
                     "visibility", "dewpoint", "solarrad", "rain", "snow",
                     "season", "holiday", "functioning")

# Change date format
bikes$calendar <- as.Date(bikes$calendar, format="%d/%m/%Y")

# Introduce variable that identifies days of the week
bikes$day <- as.factor(weekdays(bikes$calendar, abbreviate=TRUE))

# Create order of day and season
bikes$day <- ordered(bikes$day, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
bikes$season <- ordered(bikes$season, levels=c("Spring", "Summer", "Autumn", "Winter"))

# Convert day and season to factors
class(bikes$day) <- "factor"
class(bikes$season) <- "factor"

# Change holiday, functioning, and hour to Factors
bikes$holiday <- as.factor(bikes$holiday)
bikes$functioning <- as.factor(bikes$functioning)
bikes$hour <- as.factor(bikes$hour)

# Unit conversions
bikes$temp <- (bikes$temp * 9/5) + 32 # temperature degrees C to F
bikes$wind <- bikes$wind * 2.23693629 # wind speed m/s to mph
bikes$visibility <- (bikes$visibility * 10) / 1609.344 # visibility from 10m to miles
bikes$dewpoint <- (bikes$dewpoint * 9/5) + 32 # dew point temperature from degrees C to F
bikes$solarrad <- bikes$solarrad * 0.277778 # solar radiation from MJ/m^2 to kWh/m^2
bikes$rain <- bikes$rain / 25.4 # rainfall millimeters to inches
bikes$snow <- bikes$snow / 2.54 # snow centimeters to inches

# Create separate dataframe for functioning variable (plot purposes only, see Data Viz section)
#nofunct <- bikes[c("calendar", "hour", "count", "functioning")]  ## Not used in current version of paper

# Remove observations where functioning == No (no count data available for these hours)
bikes <- bikes[bikes$functioning=="Yes",]

# Remove calendar and functioning columns (irrelevant info)
bikes <- subset(bikes, select = -c(calendar, functioning)) 

# Reorder variables
bikes <- bikes[c("count", "season", "day", "hour", "holiday",
                 "temp", "humidity", "wind", "visibility", "dewpoint",
                 "solarrad", "rain", "snow")]

# Examine revised/cleaned dataset structure
#str(bikes)

# Examine data summary
summary(bikes)


###----------------------------------------------------------------------------
### Preliminary Data Visualization
### ---------------------------------------------------------------------------

# Scatterplots of numerical variables
par(mfrow=c(2,4)) # make 2x4 grid of plots
for (n in c(6:13)){
  plot(bikes[ ,n], bikes$count,   
       xlab=colnames(bikes)[n], 
       ylab="",
       col="cadetblue2",
       pch=1,
       cex.lab=2, cex.axis=2
  )
  abline(lm(bikes$count~bikes[,n]), col="lightsalmon3", lwd=2) # regression best fit line
}
par(oma = c(0, 0, 1.5, 0)) # define outer margin area
par(mfrow=c(1,1)) # return grid to 1 x 1
mtext("Bike Counts per Hour vs. Each Numerical Variable", outer=TRUE, cex=1.5) # title for grid of plots

par(opar) # restore graphical parameters

# Histogram of counts
hist(bikes$count, breaks = seq(0,3600,100), col="cadetblue2", border=FALSE,
     ylim=c(0,1200), xlab="Bikes per Hour",
     main="Histogram of Bike Rentals per Hour")

# Boxplots of Categorical Variables:
par(mfrow=c(2,2)) # make 2x2 grid of plots

## Counts vs Season
boxplot(bikes$count~bikes$season,
        xlab="", 
        ylab="Bike Count", ylim=c(0,5000),
        col=c("cadetblue2","cadetblue2","cadetblue2","lightsalmon3"),
        main="Season"
)
abline(median(bikes$count),0, col="gray", lty="dotted", lwd=3) # line at median bike count

## Counts vs Day
boxplot(bikes$count~bikes$day,
        xlab="", 
        ylab="Bike Count", ylim=c(0,5000),
        col=c("cadetblue2","cadetblue2","cadetblue2","cadetblue2","cadetblue2",
              "lightsalmon3", "lightsalmon3"),
        main="Day of the Week"
)
abline(median(bikes$count),0, col="gray", lty="dotted", lwd=3) # line at median bike count
legend("topright", legend=c("Weekday", "Weekend"),
       fill =c("cadetblue2", "lightsalmon3"), box.lty=0 )

## Counts vs Hour
boxplot(bikes$count~bikes$hour,
        xlab="", 
        ylab="Bike Count", ylim=c(0,5000),
        col=c("cadetblue2","cadetblue2","cadetblue2","cadetblue2","cadetblue2",
              "cadetblue2","cadetblue2","lightsalmon3","lightsalmon3","lightsalmon3",
              "cadetblue2","cadetblue2","cadetblue2","cadetblue2","cadetblue2",
              "cadetblue2","cadetblue2","lightsalmon3","lightsalmon3","lightsalmon3",
              "cadetblue2","cadetblue2","cadetblue2","cadetblue2","cadetblue2"),
        main="Time of Day (0 = Midnight)"
)
abline(median(bikes$count),0, col="gray", lty="dotted", lwd=3)  # line at median bike count
legend("topleft", legend=c("Non-Commuting Hours", "Commuting Hours"),
       fill =c("cadetblue2", "lightsalmon3"), box.lty=0)

## Counts vs Holiday
boxplot(bikes$count~bikes$holiday,
        xlab="", 
        ylab="Bike Count", ylim=c(0,5000),
        col=c("cadetblue2", "lightsalmon3"),
        main="Holiday or Not"
)
abline(median(bikes$count),0, col="gray", lty="dotted", lwd=3)  # line at median bike count

#Make legend to define Median line on the categorical variable box plots
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="") # blank plot to overlay
legend("center", legend="Median", col="gray", lty="dotted", lwd=3, 
       xpd=TRUE, title="Legend")
       
par(opar) # restore graphical parameters

# Plot non-functioning hours  ### not used in current version of paper
#nofunct <- transform(nofunct, count = ifelse(functioning=="Yes", 0, 1))
#nofunct <- nofunct  %>% group_by(calendar) %>% summarise(total = sum(count))
#plot(nofunct[nofunct$total > 0,], ylim=c(0,30), pch=19, col="lightsalmon3",
#     main="Daily Non-Functioning Hours", ylab="", yaxs="i",
#     xlim=c(min(nofunct$calendar), max(nofunct$calendar)),
#     xlab="Calendar: Dec 1, 2017 to Nov 30, 2018"
#)      
#segments(x0=nofunct$calendar, y0=nofunct$total, y1=0, col="gray80")



###-------------------------------------------------------------------
### Hypothesis Testing
###-------------------------------------------------------------------

#Null hypothesis: Mean # of riders on weekdays = Mean # of riders on weekends
#Alternative hypothesis: Mean # of riders on weekdays ≠ Mean # of riders on weekends

hypoData <- bikes #copying the data for the hypothesis test

hypoData$weekend = ifelse(hypoData$day %in% c("Sat", "Sun"), "weekend", "weekday") #creating new field to check if weekend

#taking a sample of 500 weekdays and 500 weekends
set.seed(17)
hypoSample <- hypoData %>% group_by(weekend) %>% sample_n(500)

#testing for equal variances
ftest <- var.test(count ~ weekend, data = hypoData)
ftest  # p-value < 0.05, reject null hyp.  accept alt hyp. vars not equal.

#testing for normality 
shapiro.test(hypoSample$count)

#splitting into 2 groups
WD = hypoSample$count[hypoSample$weekend == 'weekday']
WE = hypoSample$count[hypoSample$weekend == 'weekend']

#performing t test
myttest = t.test(WD, WE, 
                 alternative = "two.sided", 
                 paired = FALSE, 
                 mu = 0, #assuming there is no difference in means
                 var.equal = FALSE, #unequal variances
                 conf.level = 0.95) #using 95% confidence

#plotting our results with gginference package
ggttest(myttest, colaccept="cadetblue2", colreject="seagreen2", colstat="navyblue")

# display t test results
myttest


###-------------------------------------------------------------------
### Regression Analysis
###-------------------------------------------------------------------

# Copy bikes to variable for regression analysis
regData <- bikes[-1] # remove counts response variable

#Choose random indexes for assigning data to Training and Testing
set.seed(123) # set.seed for repeatable results          
nRecords <- dim(regData)[1] # count the number of records in bikes
testIndexes <- sample(nRecords, round(nRecords/5)) #randomly assign the indexes for testing
bikesTrain <- regData[-testIndexes,] #training dataframe, bikes dataset minus testIndexes
bikesTest <- regData[testIndexes,] #test dataframe, bikes dataset using testIndexes
yR <- bikes[,"count"] # define the dependent variable for regression analysis
xR <- model.matrix(yR~.,data=regData)[,-1] # define the dependent variables in matrix form
#Note: The default model.matrix() creates an (Intercept) col, the [,-1] removes this col
#For more information on model.matrix(): 
#https://stat.ethz.ch/R-manual/R-patched/library/stats/html/model.matrix.html

#Apply the random indexes to the dependent and independent variables
xR.train <- xR[-testIndexes,] # independent var training matrix, xR matrix minus testIndexes
xR.test <- xR[testIndexes,] # independent var test matrix, xR matrix using testIndexes
yR.train <- yR[-testIndexes] # dependent var training matrix, yR matrix minus testIndexes
yR.test <- yR[testIndexes] # dependent var test matrix, yR matrix using testIndexes

#Perform OLS and computer MSE to serve as Baseline:
mod.OLS <- lm(yR.train ~. ,data=bikesTrain) # linear model definition
pred.OLS <- predict(mod.OLS,bikesTest) # feed test dataframe into the linear model
#This pred object contains 'estimated' bike counts for each set of variables in the bikesTest

#OLS Model Performance Parameters
MSE.OLS <- round(mean((yR.test - pred.OLS)^2),2) # calculates the mean squared errors for OLS
RMSE.OLS <- round(sqrt(MSE.OLS),2) # calculates the root mean squared error for OLS
#mae.ols <- round(mean((abs(yR.test - pred.OLS))),2)  # mean absolute error
#RSS.OLS <- round(sum((yR.test - pred.OLS)^2),2) # calculates the sum squared errors
#Note these are all proportionately related to one another, can choose one over the others



# OLS coeeficients
coef.OLS <- round(coef(summary(mod.OLS)),2) # rounded coefficients table for report purposes
summary(mod.OLS) #to see significance levels  

#Correlation Matrix for numberical variables
corMatrix <- cor(bikes[,c("temp","humidity","wind","visibility","dewpoint","solarrad","rain","snow")])
corrplot(corMatrix, method="square",order="hclust",addrect = 2, # plot the matrix
         title="Correlation Matrix of Independent Variables",mar=c(0,0,1,0))   
par(opar) # restore graphical parameters


#Perform RIDGE Regression and compute MSE:
set.seed(123) # set.seed for repeatable results
cv.ridge <- cv.glmnet(xR.train, yR.train, alpha = 0) # cross-validation on training data to get lambdas
mod.ridge <- glmnet(xR.train, yR.train, alpha = 0, lambda = cv.ridge$lambda.1se) # model creation using lambda 1se
pred.ridge <- predict(mod.ridge, s=cv.ridge$lambda.1se, newx=xR.test) # prediction model on the test data

#Create cross-validation plot to visualize values of lambda.min and lambda.1se
plot(cv.ridge)
title("Cross-Validation Plot to Determine Lambda.min and Lambda.1se", line = 3) # add title for context with spacing 3

#RIDGE Model Performance Parameters
MSE.ridge <- round(mean((yR.test - pred.ridge)^2),2) # calculates the mean squared errors for RIDGE
RMSE.ridge <- round(sqrt(MSE.ridge),2) # calculates the root mean squared error for RIDGE
#mae.ridge <- round(mean((yR.test - abs(pred.ridge))),2) # mean absolute error
#RSS.ridge <- round(sum((yR.test - pred.ridge)^2),2) # calculates the sum squared errors
#Note these are all proportionately related to one another, can choose one over the others

coef.ridge <- round(as.matrix(coef(cv.ridge,s=cv.ridge$lambda.1se)),2) # rounded coefficients table

#Perform LASSO Regression and computer MSE:
set.seed(123) # set.seed for repeatable results
plot(glmnet(xR.train, yR.train, alpha = 1),xvar="lambda") #plot lasso prior to defining lambda
title("Lasso Coefficient Trace Plot", line = 3)
cv.lasso <- cv.glmnet(xR.train, yR.train, alpha = 1) # cross-validation on training data to get lambdas
mod.lasso <- glmnet(xR.train, yR.train, alpha = 1, lambda = cv.lasso$lambda.1se) # model creation using lambda 1se
pred.lasso <- predict(mod.lasso, s=cv.lasso$lambda.1se, newx=xR.test) # prediction model on the test data

#LASSO Model Performance Parameters
MSE.lasso <- round(mean((yR.test - pred.lasso)^2),2) # calculates the mean squared errors for LASSO
RMSE.lasso <- round(sqrt(MSE.lasso),2) # calculates the root mean squared error for LASSO
#mae.lasso <- round(mean((yR.test - abs(pred.lasso))),2) # mean absolute error
#RSS.lasso <- round(sum((yR.test - pred.lasso)^2),2) # calculates the sum squared errors
#Note these are all proportionately related to one another, can choose one over the other two

coef.lasso <- round(as.matrix(coef(cv.lasso,s=cv.lasso$lambda.1se)),2) # rounded coefficients table 


###-------------------------------------------------------------------
### ELASTIC NET LOOP
###-------------------------------------------------------------------

#As a means of trying to "optimize" which alpha value creates the best model
#     this section will loop through lambda.min and 1se for values of alpha
#     ranging from 0 to 1, in 0.01 increments:

loop.result <- data.frame() # create a blank dataframe to be used in the loop
for (a in seq(0, 1, .01)){  # loop through a values from 0 to 1 in 0.01 increments
  set.seed(123)             # set seed for repeatability
  cv.loop <- cv.glmnet(xR.train, yR.train, alpha = a)     # perform cross-validation to determine lambda values
  for (val in c(cv.loop$lambda.1se)){  # for both lambda.min and lambda.1se loop through models
    mod.loop <- glmnet(xR.train, yR.train, alpha = a, lambda = val) # create model object using loop values for a and lambda
    pred.loop <-  predict(mod.loop, s=val, newx=xR.test) # apply model to test data
    MSE.loop <- round(mean((yR.test - pred.loop)^2),2) # calculate differences between test and actual values
    RMSE.loop <- round(sqrt(MSE.loop),2) # calculate RMSE based on MSE
    loop.result <- rbind(loop.result,c(a,round(val,2),RMSE.loop,round(100*(RMSE.OLS-RMSE.loop)/RMSE.OLS,3))) #create a result table
  }
}
colnames(loop.result)<-c("alpha","lambda","RMSE","%Improvement") # assign headers to the result table
loop.result <- loop.result[order(loop.result$RMSE),] # sort the table by lowest RSME
head(loop.result,5)   # return the five model parameters coinciding with the lowest RMSE

# recreate a model based on parameters coinciding with the lowest RSME
mod.enet <- glmnet(xR.train, yR.train, 
                   alpha = loop.result[1,1],
                   lambda = loop.result[1,2])

# Predictions on test data from final elastic net model
pred.enet <-  predict(mod.enet, s=val, newx=xR.test)

#to alleviate any issues from using rounded values for alpha and lambda, return the lowest RMSE from the table
RMSE.enet <- min(loop.result$RMSE) 
#mae.enet <- round(mean((yR.test - abs(pred.enet))),2) # mean absolute error # need to calc mae in loop

###-------------------------------------------------------------------
### Regularization Summary
###-------------------------------------------------------------------

#Results Table:  
reg.results = data.frame(
  "Model" = c("OLS", "Ridge Regression", "Lasso","Elastic Net"), 
  "RMSE" = c(RMSE.OLS,RMSE.ridge,RMSE.lasso,RMSE.enet))
reg.results


# Variable importance plots for regression models
vip.lasso <- vip(mod.lasso,num_features=41) + ggtitle("Lasso Model Predictors")
vip.ridge <- vip(mod.ridge,num_features=41) + ggtitle("Ridge Model Predictors")
vip.OLS <- vip(mod.OLS,num_features=41) + ggtitle("OLS Model Predictors")
vip.enet <- vip(mod.enet,num_features=41) + ggtitle("Elastic Net Model Predictors")
grid.arrange(vip.OLS,vip.ridge,vip.lasso, vip.enet, ncol=4)


###-------------------------------------------------------------------
### Random Forest Regression Model
###-------------------------------------------------------------------

# Create new dataframe for random forest analysis
rf.bikes <- bikes

# Divide data into train and test sets
set.seed(123) # set.seed for repeatable results          
testrows.rf <- sample(nrow(rf.bikes), round(nrow(rf.bikes)/5)) #randomly assign rows/observations for testing
rf.train <- rf.bikes[-testrows.rf,] # train dataframe, only non-"testrows"
rf.test <- rf.bikes[testrows.rf,] #test dataframe, only testrows

# New objects for storing error values from optimization loop
oob.err <- double(ncol(rf.bikes)-1)  # new object for storing Out of Bag errors

######  Loop to find optimal value of mtry - WARNING: this takes a few minutes ###
for (mtry in 1:(ncol(rf.bikes)-1) ){
  set.seed(42)
  fit.rf <- randomForest(count ~ ., data=rf.train, mtry=mtry, ntree=100) # reduce ntree to faster loop
  oob.err[mtry] <- sqrt(fit.rf$mse[100])
}

# What are the RMSE and mtry values associated w/ the best mtry (min RMSE)
best.mtry <- which(oob.err==min(oob.err))  # find which model gives min test error
rmse.best.mtry <- oob.err[best.mtry]   # depending on set.seed, seems to be 9 or 10

par(mfrow=c(1,2)) # make 1x2 grid of RF plots (mtry and ntree)

# Plot RMSE from OOB vs mtry value
plot(1:mtry, oob.err, 
     pch = 23,
     cex=ifelse(oob.err==rmse.best.mtry, 2, 1),
     bg=ifelse(oob.err==rmse.best.mtry, "red", "white"),
     col=ifelse(oob.err==rmse.best.mtry, "red", "blue"),
     type = "b", 
     ylab="OOB Root Mean Squared Error",
     xlab="mtry",
     main="OOB RMSE vs. Value of mtry",
     lwd=2
)

# Rerun Random Forest with best.mtry, larger number of trees, and importance=TRUE  # takes a few minutes
set.seed(42)
rf.model <- randomForest(count ~ ., data=rf.train, 
                         mtry=best.mtry, ntree=500, importance=TRUE) 


# Revise default plot to use RMSE instead of MSE
plot(1:rf.model$ntree, sqrt(rf.model$mse), type="l", col="blue", lwd=2,
     main="OOB RMSE vs. Number of Trees",
     xlab="ntree",
     ylab="OOB Root Mean Squared Error",
     ylim=c(180,280)
     )
# Plot shows error decreasing with number of trees.  Looks pretty stable by 300

par(mfrow=c(1,1)) # return plotting to normal 1x1 layout

# # Modified plots for presentation ##
# par(mar=c(5,6,4,2))
# plot(1:mtry, oob.err,
#      pch = 23,
#      cex=ifelse(oob.err==rmse.best.mtry, 3, 1.5),
#      bg=ifelse(oob.err==rmse.best.mtry, "red", "white"),
#      col=ifelse(oob.err==rmse.best.mtry, "red", "blue"),
#      type = "b",
#      ylab="OOB Root Mean Squared Error",
#      xlab="mtry",
#      main="OOB RMSE vs. Value of mtry",
#      lwd=2,
#      cex.main=2,
#      cex.lab=2,
#      cex.axis=2,
# )
# #Revised plot for presentation
# plot(1:rf.model$ntree, sqrt(rf.model$mse), type="l", col="blue", lwd=2,
#      main="OOB RMSE vs. Number of Trees",
#      xlab="ntree",
#      ylab="OOB Root Mean Squared Error",
#      cex.main=2, cex.axis=2, cex.lab=2
# )
# par(opar) # Restore original graphics parameters


# variable importance plot, shows which vars have larger effect on the model
varImpPlot(rf.model, 
           main="Random Forest Variable Importance Plots",
           pch=19, col="blue")

# # modified variable imp plot for presentation
# varImpPlot(rf.model,
#            main="",
#            pch=19, col="blue")


# Final Random Forest regression model
print(rf.model)

# Input test data into Random Forest Model
pred.rf <- predict(rf.model, rf.test)

#RF Model Performance Parameters
mse.rf <- round(mean((rf.test$count - pred.rf)^2),2) # calculates the mean squared errors for RF
rmse.rf <- round(sqrt(mse.rf), 2) # calculates the root mean squared error for RF  
#mae.rf <- round(mean((abs(yR.test - pred.rf))),2) # mean absolute error
#rss.rf <- round(sum((rf.test$count - pred.rf)^2),2) # calculates the sum squared errors for RF


###-------------------------------------------------------------------
### Compare Regression Models
###-------------------------------------------------------------------

# Comparison of model errors
err.results = data.frame(
  "Model" = c("OLS Model", "Ridge Regression Model", "Lasso Model", "Elastic Net Model","Random Forest"), 
  "RMSE" = c(RMSE.OLS,RMSE.ridge,RMSE.lasso, RMSE.enet, rmse.rf)
  )
err.results


# Plot Predicted values vs Actual Values
layout(matrix(c(1,1,2,2,3,3,4,4,0,5,5,0), ncol=4, byrow=TRUE))  # create layout matrix for next 5 plots

# OLS
plot(yR.test, pred.OLS, col="blue", 
     xlim=c(0,3500), ylim=c(0,3500),
     main="OLS Model",
     xlab="Actual Test Value of Count",
     ylab="Predicted Value from Model"
)
abline(0,1, lty="dotted") # Line indicated Predicted = Actual
abline(lm(pred.OLS~yR.test), col="blue", lwd=3) # best fit line of model
text(3000, 500, paste("RMSE = ", RMSE.OLS), cex=1.5) # RMSE of model

# Ridge
plot(yR.test, pred.ridge, col="blue", 
     xlim=c(0,3500), ylim=c(0,3500),
     main="Ridge Model",
     xlab="Actual Test Value of Count",
     ylab="Predicted Value from Model"
)
abline(0,1, lty="dotted") # Line indicated Predicted = Actual
abline(lm(pred.ridge~yR.test), col="blue", lwd=3) # best fit line of model
text(3000, 500, paste("RMSE = ", RMSE.ridge), cex=1.5) # RMSE of model

# Lasso
plot(yR.test, pred.lasso, col="blue", 
     xlim=c(0,3500), ylim=c(0,3500),
     main="LASSO Model",
     xlab="Actual Test Value of Count",
     ylab="Predicted Value from Model"
)
abline(0,1, lty="dotted") # Line indicated Predicted = Actual
abline(lm(pred.lasso~yR.test), col="blue", lwd=3) # best fit line of model
text(3000, 500, paste("RMSE = ", RMSE.lasso), cex=1.5) # RMSE of model

# Elastic Net  
plot(yR.test, pred.enet, col="blue", 
     xlim=c(0,3500), ylim=c(0,3500),
     main="Elastic Net Model",
     xlab="Actual Test Value of Count",
     ylab="Predicted Value from Model"
)
abline(0,1, lty="dotted") # Line indicated Predicted = Actual
abline(lm(pred.enet~yR.test), col="blue", lwd=3) # best fit line of model
text(3000, 500, paste("RMSE = ", RMSE.enet), cex=1.5) # RMSE of model

# Random Forest
plot(rf.test$count, pred.rf, 
     col="blue", 
     xlim=c(0,3500), ylim=c(0,3500),
     main="Random Forest Model",
     xlab="Actual Test Value of Count",
     ylab="Predicted Value from Model"
)
abline(0,1, lty="dotted") # Line indicated Predicted = Actual
abline(lm(pred.rf~rf.test$count), col="blue", lwd=3) # best fit line of model
text(3000, 500, paste("RMSE = ", rmse.rf), cex=1.5) # RMSE of model

layout(1) # return plot parameter to just one at a time


###-------------------------------------------------------------------
### Random Forest Classification Model
###-------------------------------------------------------------------

# Create new dataframe for random forest classification
bclass <- bikes

# Calculate cutoffs for 15th percentile and 85th percentile
div <- quantile(bclass$count, c(0.15,0.85))

# div # 15%: 137; 85%: 1424.4


# Create new column for "demand" split at 15th/85th percentile and named low, mid, and high
bclass$demand <- cut(bclass$count, breaks=c(0,div[1],div[2], max(bclass$count)), 
                     labels=c("low", "mid", "high"), include.lowest = TRUE)

# Remove count variable, since its directly correlated to demand level
bclass <- subset(bclass, select= -count)

# Split data into train and test sets
set.seed(123) # set seed for reproducibility
testrows.bclass <- sample(nrow(bclass), round(nrow(bclass)/5)) #randomly assign rows/observations for testing
bclass.train <- bclass[-testrows.bclass,] # train dataframe, only non-"testrows"
bclass.test <- bclass[testrows.bclass,] #test dataframe, only testrows

# New objects for storing error values from optimization loop
ooberr.rfcl <- double(ncol(bclass)-1)  # new object for storing estimated Out of Bag errors

# Loop for optimizing mtry
for (mtrycl in 1:(ncol(bclass)-1) ){
  set.seed(123)
  fit <- randomForest(demand ~ ., data=bclass.train, mtry=mtrycl, ntree=100) # reduce ntree to run faster
  ooberr.rfcl[mtrycl] <- fit$err.rate[fit$ntree]
}

# What is the oob error and mtry values associated w/ the best mtry (min err)
bestmtry.rfcl <- which(ooberr.rfcl==min(ooberr.rfcl))  # find which model gives min test error
errbestmtry.rfcl <- ooberr.rfcl[bestmtry.rfcl] 


par(mfrow=c(1,2)) # make 1x2 grid of RF plots (mtry and ntree)

# Plot estimate of error rate from OOB vs mtry value
plot(1:mtrycl, ooberr.rfcl, 
     pch = 23,
     cex=ifelse(ooberr.rfcl==errbestmtry.rfcl, 2, 1),
     bg=ifelse(ooberr.rfcl==errbestmtry.rfcl, "red", "white"),
     col=ifelse(ooberr.rfcl==errbestmtry.rfcl, "red", "blue"),
     type = "b", 
     ylab="OOB Estimate of Error Rate",
     xlab="Value of mtry",
     main="OOB Estimate of Error Rate vs. Value of mtry",
     lwd=2
)

# Run final classification random forest model
set.seed(123)
rfclass <- randomForest(demand~., data=bclass.train, mtry=bestmtry.rfcl, ntree=500, type=classification, importance=TRUE)


# Plot model
plot((1:rfclass$ntree), rfclass$err.rate[,1], type="l", lwd=2, col="black",
     main="Estimate of Error vs. Number of Trees in Model",
     xlab="ntree",
     ylab="Estimate of Error",
     ylim=c(0.05,0.35)
)
lines((1:rfclass$ntree), rfclass$err.rate[,2], lwd=2, col="red", lty="dashed")
lines((1:rfclass$ntree), rfclass$err.rate[,3], lwd=2, col="green", lty="dotted")
lines((1:rfclass$ntree), rfclass$err.rate[,4], lwd=2, col="blue", lty="dotdash")
# error looks like it's stabilized at a low level
# add legend
legend("topright", colnames(rfclass$err.rate), col=1:4, lty=1:4, lwd=2) # add legend

layout(1)

# par(mar=c(5,6,4,2))
# # Plot estimate of error rate from OOB vs mtry value (revised for presentation)
# plot(1:mtrycl, ooberr.rfcl,
#      pch = 23,
#      cex=ifelse(ooberr.rfcl==errbestmtry.rfcl, 3, 1.5),
#      bg=ifelse(ooberr.rfcl==errbestmtry.rfcl, "red", "white"),
#      col=ifelse(ooberr.rfcl==errbestmtry.rfcl, "red", "blue"),
#      type = "b",
#      ylab="OOB Estimate of Error Rate",
#      xlab="Value of mtry",
#      main="OOB Estimate of Error Rate vs. Value of mtry",
#      lwd=2, cex.main=2, cex.lab=2, cex.axis=2
# )
# # Plot model (revised for presentation)
# plot((1:rfclass$ntree), rfclass$err.rate[,1], type="l", lwd=2, col="black",
#      main="Estimate of Error vs. Number of Trees in Model",
#      xlab="ntree",
#      ylab="Estimate of Error",
#      ylim=c(0.05,0.35),
#      cex.main=2, cex.axis=2, cex.lab=2
# )
# lines((1:rfclass$ntree), rfclass$err.rate[,2], lwd=2, col="red", lty="dashed", cex=2)
# lines((1:rfclass$ntree), rfclass$err.rate[,3], lwd=2, col="green", lty="dotted", cex=2)
# lines((1:rfclass$ntree), rfclass$err.rate[,4], lwd=2, col="blue", lty="dotdash", cex=2)
# # error looks like it's stabilized at a low level
# # add legend
# legend("topright", colnames(rfclass$err.rate), col=1:4, lty=1:4, lwd=2, cex=1.3) # add legend
# 
# par(opar) # Restore original graphics parameters


# Print model summary
print(rfclass)

# variable importance plot for RF classification model.
varImpPlot(rfclass, scale=FALSE, main="Random Forest Variable Importance Plots",  
           pch=19, col="blue")

## variable importance plot for RF classification model. modified for presentation
#varImpPlot(rfclass, scale=FALSE, main="",  
#           pch=19, col="blue")


# Input test data into Random Forest Model
predcl <- predict(rfclass, bclass.test)

# Confusion Matrix / Accuracy of predicted results
results <- table(bclass.test[,"demand"], predcl)  # make table of predicted results
#results
class.error <- c(0,0,0)  # dummy values
class.error[1] <- (results[1,2] + results[1,3]) / rowSums(results[,1:3])[1] # Type 1 class error
class.error[2] <- (results[2,1] + results[2,3]) / rowSums(results[,1:3])[2] # Type 2 class error
class.error[3] <- (results[3,1] + results[3,2]) / rowSums(results[,1:3])[3] # Type 3 class error
results <- cbind(results, class.error) # combine class.error into results table
rownames(results) <- c("Actual low", "Actual mid","Actual high") # rename rows
print(results) # display

# Calculate Model Prediction Accuracy
acc <- (results[1,1] + results[2,2] + results[3,3]) / sum(results)
noquote(paste("Model Prediction Accuracy on Test Data is ", round(acc, 4))) # print result

# Restore original graphics parameters [LAST LINE OF CODE]
par(opar)

