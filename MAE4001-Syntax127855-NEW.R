##############################
## Utilities
##############################
### 	Load packages 
library(tidyverse)
library(dplyr)
library(foreign)
library(ggplot2)
library(ggExtra)
library(car)
library(moments)
library(psych)

###	Custom functions that is used later on in the reporting and analysis
desc<-function(x){
  stat = c(M=mean(x,na.rm=T),SD=sd(x,na.rm=T),
           skewness=skewness(x,na.rm=T),excess.kurtosis=
             kurtosis(x,na.rm=T)-3,n.eff=sum(!is.na(x)),n=
             length(x))
  return(stat)
}

regCoef<-function(model,data,q=.95,digits=3){
  sm = summary(model)
  ci = confint(model, level=q)
  D = cbind(model$model[,1],model.matrix(model,data)[,-1])
  S = cov(D,use="complete.obs")
  Sd = sqrt(diag(S))
  BetaZ = coef(model)*c(0,Sd[-1]/Sd[1])
  R = cov2cor(S)
  Tol.x = c(NA,1/diag(qr.solve(R[-1,-1])))
  Ry.x = c(NA,R[-1,1])
  P = qr.solve(R)
  S.P = diag(1/sqrt(diag(P)))
  Ryx.x = c(NA,-(S.P%*%P%*%S.P)[-1,1])
  Ry.x.x = BetaZ*sqrt(Tol.x)
  tab = cbind(sm$coefficients,ci,BetaZ,Tol.x,Ry.x,
              Ryx.x,Ry.x.x)
  colnames(tab)=c("b","SE","t","p","[b.l,","b.u]","
b_Z","Tol","Ryx","Ryx.x","Ry(x.x)")
  return(round(tab,digits))
}

#Function to flag outliers
influ.out<-function(m){
  
  Yhat = fitted.values(m)
  Ehat = residuals(m)
  Leverage = hatvalues(m)
  EhatT = rstudent(m)
  CooksD = cooks.distance(m)
  
  n = length(Yhat)
  J = length(coef(m))	
  
  data = data.frame(Yhat=Yhat,Ehat=Ehat,
                    Leverage=Leverage,Leverage.flag=Leverage>(2*(J+1)/n),
                    EhatT=EhatT,EhatT.flag = abs(EhatT)>2,
                    CooksD=CooksD,CooksD.flag=CooksD>(4/n) )
  return(data)
}

##############################
## Data: 
##############################
###	Importing dataset 
data <- read.table("DATA-127855.txt", header = TRUE, dec = ".")
View(data)
str(data)

# Convert non-numeric variables to numeric
data$PISA <- as.numeric(data$PISA)
data$BREASTFEEDING <- as.numeric(data$BREASTFEEDING)

###	Create and/or rescale variables for latter use and interpretation
#Centering each predictor as well as variable GDP
data$INHABITANTS_C <- data$INHABITANTS-mean(data$INHABITANTS,na.rm=T)
data$FISH_C <- data$FISH - mean(data$FISH, na.rm = T)
data$BREASTFEEDING_C <- data$BREASTFEEDING - mean (data$BREASTFEEDING, na.rm=T)
data$INTERNETCOVERAGE_C <- data$INTERNETCOVERAGE-mean(data$INTERNETCOVERAGE,na.rm=T)
data$GDP_C <- data$GDP - mean(data$GDP, na.rm = T) 

#Creating a categorical predictor that split countries into low/high GDP countries by re-coding the `GDP` variable
#I am using the median as a threshold to create a binary split
median_inhabitants <- median(data$GDP)

data <- data %>%
  mutate(GDPLEVEL = ifelse(GDP > median_inhabitants, "HIGH", "LOW"))

#This creates a new column called GDPLEVEL in the dataset,
#where countries with lower GDP than the median are labeled as "Low"
#and countries with higher GDP than the median are labeled as "High"

#Converting categorical predictor to numerical, where 1= high and 0=low
data$GDPLEVEL <- as.numeric(data$GDPLEVEL == "HIGH")

#Creating a second-order condition of INHABITANTS_C*GDP_C
data$INHABITANTS_GDP = data$INHABITANTS_C*data$GDP_C

##############################
## Descriptive statistics:
##############################
###	Computation of statistics

###	Collect information: Create object(s) that correspond to what you end up reporting in text

#Univariate descriptive statistics
Min <- sapply(data[c("PISA", "INHABITANTS", "FISH", "BREASTFEEDING", "INTERNETCOVERAGE", "GDP")], min, na.rm = TRUE)
Max <- sapply(data[c("PISA", "INHABITANTS", "FISH", "BREASTFEEDING", "INTERNETCOVERAGE", "GDP")], max, na.rm = TRUE)
Median <- sapply(data[c("PISA", "INHABITANTS", "FISH", "BREASTFEEDING", "INTERNETCOVERAGE", "GDP")], median, na.rm = TRUE)
Mean <- sapply(data[c("PISA", "INHABITANTS", "FISH", "BREASTFEEDING", "INTERNETCOVERAGE", "GDP")], mean, na.rm = TRUE)
SD <- sapply(data[c("PISA", "INHABITANTS", "FISH", "BREASTFEEDING", "INTERNETCOVERAGE", "GDP")], sd, na.rm = TRUE)
Skewness <- sapply(data[c("PISA", "INHABITANTS", "FISH", "BREASTFEEDING", "INTERNETCOVERAGE", "GDP")], skew, na.rm=T)
Kurtosis <- sapply(data[c("PISA", "INHABITANTS", "FISH", "BREASTFEEDING", "INTERNETCOVERAGE", "GDP")], kurtosi, na.rm=T)

table_univariate <- data.frame(
  Min = round(Min, 2),
  Max = round(Max, 2),
  Median = round(Median,2),
  Mean = round(Mean, 2),
  SD = round(SD, 2),
  Skewness = round(Skewness, 2),
  Kurtosis = round(Kurtosis, 2)
)

# Save the table as a CSV file
write.csv2(table_univariate, "table_univariate.csv", row.names=TRUE)

#Bivariate descriptive statistics
table_bivariate <- round(cor(data[c("PISA", "INHABITANTS_C", "FISH_C", "BREASTFEEDING_C", "INTERNETCOVERAGE_C", "GDP_C", "INHABITANTS_GDP", "GDPLEVEL")], method="pearson"),2)

# Convert table_bivariate to a data frame
table_bivariate <- as.data.frame(table_bivariate)

# Save the table as a CSV file
write.csv2(table_bivariate, "table_bivariate.csv", row.names = TRUE)

##############################
## Linear Model:
##############################
### 	Defining and Estimating the model and related statistics

#Fitting a linear model with PISA as the outcome variable
#Predictors: INHABITANTS_C, FISH_C, BREASTFEEDING_C, INTERNETCOVERAGE_C
#plus the second-order condition (INHABITANTS_GDP) and the categorical predictor GDPLEVEL
model <- lm(PISA ~ INHABITANTS_C + FISH_C + BREASTFEEDING_C + INTERNETCOVERAGE_C + INHABITANTS_GDP + GDPLEVEL, data = data)

#Checking model summary
summary(model)
regCoef(model, data)

### 	Verifying and Dealing with Outliers / Assumptions
#Outliers

# Set up the output to a PNG file
png("outliers_plot.png", width = 800, height = 600)

#Flagging outliers
OUT = influ.out(model)

#Using boxplots to flag outliers
#Identifying the most extreme cases for each of our three key diagnostics
n = nrow(data)
J = length(coef(model))	
par(mfrow = c(1, 3))
boxplot(OUT$Leverage, xlab = "Observations", ylab = "Leverage", main = "Leverage Plot"); abline(h = 2 * (J + 1) / n, col = "blue")
boxplot(OUT$EhatT, xlab = "Observations", ylab = "Standardized Residuals", main = "Residuals Plot"); abline(h = 2, col = "blue")
boxplot(OUT$CooksD, xlab = "Observations", ylab = "Cook's Distance", main = "Cook's Distance Plot"); abline(h = 4 / n, col = "blue")

# Turn off the PNG device
dev.off()

# Identify outliers in Cook's distance
outliers_cooks <- which(OUT$CooksD > 4 / n)

# Print outlier countries and reasons
if(length(outliers_cooks) > 0) {
  cat("Outliers in Cook's distance:\n")
  for(outlier_index in outliers_cooks) {
    cat("Country:", data$Country[outlier_index], "\n")
    cat("Reason: Cook's distance is higher than the threshold (4/n).\n\n")
  }
} else {
  cat("No outliers found in Cook's distance.\n")
}

# Identify outliers in Leverage
outliers_leverage <- which(OUT$Leverage > (2 * (J + 1) / n))

# Identify outliers in Standardized Residuals
outliers_ehatT <- which(abs(OUT$EhatT) > 2)

# Print outlier countries and reasons
if(length(outliers_leverage) > 0) {
  cat("Outliers in Leverage:\n")
  for(outlier_index in outliers_leverage) {
    cat("Country:", data$Country[outlier_index], "\n")
    cat("Reason: Leverage is higher than the threshold (2*(J+1)/n).\n\n")
  }
} else {
  cat("No outliers found in Leverage.\n")
}

if(length(outliers_ehatT) > 0) {
  cat("Outliers in Standardized Residuals:\n")
  for(outlier_index in outliers_ehatT) {
    cat("Country:", data$Country[outlier_index], "\n")
    cat("Reason: Absolute value of Standardized Residuals is higher than 2.\n\n")
  }
} else {
  cat("No outliers found in Standardized Residuals.\n")
}


#Looking at countries that have the highest/lowest value of each predictor as well as the variable GDP
# Find the index of the country with the lowest GDP
min_gdp_index <- which.min(data$GDP)
cat("Country with the lowest GDP:", data$Country[min_gdp_index], "\n")

# Find the index of the country with the highest GDP
max_gdp_index <- which.max(data$GDP)
cat("Country with the highest GDP:", data$Country[max_gdp_index], "\n")

# Find the index of the country with the lowest number of inhabitants
min_inhabitants_index <- which.min(data$INHABITANTS)
cat("Country with the lowest number of inhabitants:", data$Country[min_inhabitants_index], "\n")

# Find the index of the country with the highest number of inhabitants
max_inhabitants_index <- which.max(data$INHABITANTS)
cat("Country with the highest number of inhabitants:", data$Country[max_inhabitants_index], "\n")

# Find the index of the country with the lowest fish consumption
min_fish_index <- which.min(data$FISH)
cat("Country with the lowest fish consumption:", data$Country[min_fish_index], "\n")

# Find the index of the country with the highest fish consumption
max_fish_index <- which.max(data$FISH)
cat("Country with the highest fish consumption:", data$Country[max_fish_index], "\n")

# Find the index of the country with the lowest breastfeeding rate
min_breastfeeding_index <- which.min(data$BREASTFEEDING)
cat("Country with the lowest breastfeeding rate:", data$Country[min_breastfeeding_index], "\n")

# Find the index of the country with the highest breastfeeding rate
max_breastfeeding_index <- which.max(data$BREASTFEEDING)
cat("Country with the highest breastfeeding rate:", data$Country[max_breastfeeding_index], "\n")

# Find the index of the country with the lowest internet coverage
min_internetcoverage_index <- which.min(data$INTERNETCOVERAGE)
cat("Country with the lowest internet coverage:", data$Country[min_internetcoverage_index], "\n")

# Find the index of the country with the highest internet coverage
max_internetcoverage_index <- which.max(data$INTERNETCOVERAGE)
cat("Country with the highest internet coverage:", data$Country[max_internetcoverage_index], "\n")

#Resetting plotting layout
par(mfrow = c(1, 1))

#Creating a new data for the data after removing outliers flagged by the Cook's Distance method
data.without <- data[!OUT$CooksD.flag, ]

#Analysis of model with and without flagged outliers

#Fitting a model after removing flagged outliers
model.without = lm(PISA ~ INHABITANTS_C + FISH_C + BREASTFEEDING_C + INTERNETCOVERAGE_C + INHABITANTS_GDP + GDPLEVEL, data=data.without)

#Printing both models - before and after removing outliers
summary(model)
#Findig Tol and Semi-partial correlations
regCoef(model, data)
summary(model.without)
#Findig Tol and Semi-partial correlations
regCoef(model.without, data)

#Linear model assumption 1 - Checking normality of distribution Y|X (and hence also of ε)
#We do this by checking normality of residuals

#Creating a histogram of the residuals to check if these residuals are approximately normally distributed
# Set the size of the plotting device
png("histogram1.png", width = 800, height = 600)
# Create the histogram with titles
hist(resid(model), main = "Histogram of Residuals - Before Removing Outliers", xlab = "Residuals", ylab = "Frequency")
# Save the plot to a file
dev.off()

# Set the size of the plotting device
png("histogram2.png", width = 800, height = 600)
# Create the histogram with titles
hist(resid(model.without), main = "Histogram of Residuals - After Removing Outliers", xlab = "Residuals", ylab = "Frequency")
# Save the plot to a file
dev.off()

#Linear model assumption 2 - Checking homoscedasticity (constant variance) over X for Y|X (and ε)

#We check this by creating a scatterplot of the predicted values against the residual values
#We want to see if the residuals are randomly distributed or not

#Ggplot2
#For model before outliers are removed
# Set the name of the png file
png("ggplot1.png")
ggplot(data, aes(x = fitted.values(model), y = residuals(model))) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope = 0, intercept = 0) +
  ggtitle("Predicted Values Against Residual Values - Before Removing Outliers") +
  xlab("Fitted Values") +
  ylab("Residuals")
#Save the plot to a file
dev.off()

#For model after outliers are removed
# Set the name of the png file
png("ggplot2.png")
ggplot(data.without, aes(x = fitted.values(model.without), y = residuals(model.without))) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope = 0, intercept = 0) +
  ggtitle("Predicted Values Against Residual Values - After Removing Outliers") +
  xlab("Fitted Values") +
  ylab("Residuals")
#Save the plot to a file
dev.off()

#Linear model assumption 3 - Checking form of relationship between Y and X is properly specified! (linear in predictor)
#We do this by creating scatterplots to visually inspect the relationship between the outcome variable and each of the predictors

#For model and data before outliers are removed
# Set the name of the png file
png("plot1.png")
# Set up a 3x2 layout for the plot
par(mfrow = c(3, 2))
plot(data$INHABITANTS, model$residuals, main="Residuals vs. INHABITANTS", xlab="INHABITANTS", ylab="Residuals")
plot(data$FISH, model$residuals, main="Residuals vs. FISH", xlab="FISH", ylab="Residuals")
plot(data$BREASTFEEDING, model$residuals, main="Residuals vs. BREASTFEEDING", xlab="BREASTFEEDING", ylab="Residuals")
plot(data$INTERNETCOVERAGE, model$residuals, main="Residuals vs. INTERNETCOVERAGE", xlab="INTERNETCOVERAGE", ylab="Residuals")
plot(data$INHABITANTS_GDP, model$residuals, main="Residuals vs. INHABITANTS_GDP", xlab="INHABITANTS_GDP", ylab="Residuals")
#Save the plot to a file
dev.off()

#For model and data after outliers are removed
# Set the name of the png file
png("plot2.png")
# Set up a 3x2 layout for the plot
par(mfrow = c(3, 2))
plot(data.without$INHABITANTS, model.without$residuals, main="Residuals vs. INHABITANTS - After Removing Outliers", xlab="GDP", ylab="Residuals")
plot(data.without$FISH, model.without$residuals, main="Residuals vs. FISH - After Removing Outliers", xlab="FISH", ylab="Residuals")
plot(data.without$BREASTFEEDING, model.without$residuals, main="Residuals vs. BREASTFEEDING - After Removing Outliers", xlab="BREASTFEEDING", ylab="Residuals")
plot(data.without$INTERNETCOVERAGE, model.without$residuals, main="Residuals vs. INTERNETCOVERAGE - After Removing Outliers", xlab="INTERNETCOVERAGE", ylab="Residuals")
plot(data.without$INHABITANTS_GDP, model.without$residuals, main="Residuals vs. INHABITANTS_GDP - After Removing Outliers", xlab="FISH_GDP", ylab="Residuals")
#Save the plot to a file
dev.off()

#CREATING A SIMPLE-SLOPE PLOT for supporting the interpretation of the variables included in the second-order term
m3 <- lm(PISA~INHABITANTS_C*GDP_C, data)
summary(m3)

pseudos = data.frame(INHABITANTS_C=rep(c(min(data$INHABITANTS_C),max(data$INHABITANTS_C)),5), GDP_C=rep(c(min(data$GDP_C),mean(data$GDP_C)-sd(data$GDP_C),mean(data$GDP_C),mean(data$GDP_C)+sd(data$GDP_C),max(data$GDP_C)),each=2))
pseudos = cbind(predict(m3,pseudos,interval ="confidence"),pseudos)
names(pseudos)[1]="PISA" 
pseudos 

#Simple-slope plot 1
# Set the size of the plotting device
png("simpleslope.png")

b = coef(m3)
X = with(data,rbind(
  c(min(INHABITANTS_C), min(GDP_C)),
  c(max(INHABITANTS_C), min(GDP_C)),
  c(min(INHABITANTS_C), mean(GDP_C)),
  c(max(INHABITANTS_C), mean(GDP_C)),
  c(min(INHABITANTS_C), max(GDP_C)),
  c(max(INHABITANTS_C), max(GDP_C))
))
X = cbind(1, X, X[, 1] * X[, 2])

yhat = X %*% b
par(bty = "l", mar = c(5, 4, 4, 2))
plot(X[1:2, 2], yhat[1:2], type = "l", lwd = 3, col = "red", xlab = "INHABITANTS_C (Millions)", ylab = "PISA - Predicted PISA Mean Achievement Score of Students in Country", ylim = range(yhat))
lines(X[3:4, 2], yhat[3:4], lwd = 3, col = "black")
lines(X[5:6, 2], yhat[5:6], lwd = 3, col = "green")
legend(1, 1, legend = c("min(GDP_C)", "mean(GDP_C)", "max(GDP_C)"), lwd = rep(3, 3), col = c("red", "black", "green"))
legend("right", legend = c("min(GDP_C)", "mean(GDP_C)", "max(GDP_C)"), lwd = rep(3, 3), col = c("red", "black", "green"))

#Save the plot to a file
dev.off()
