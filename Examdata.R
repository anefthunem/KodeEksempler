#Loading the data
data = read.table("Abadi et al. (2023) A Dataset of Social-Psychological and Emotional Reactions during the COVID-19 Pandemic across Four European Countries.csv",sep=",",header=TRUE,dec=".")

#Loading packages
library(mirt)
library(dplyr)
library(e1071)
library(psych)

#Filtering the dataset
data <- data[data$A3.1 != 3, ]

#Making a new dataset with my chosen items
# Specify column names to keep
columns_to_keep <- c("RespondentID", "A3.1",
                     "A24.1", "A24.2", "A24.3", "A24.4", 
                     "A24.5", "A24.6")

# Create a new dataset with only the specified columns
mydata <- data[, columns_to_keep]
#removing gender=other
mydata <- mydata[mydata$A3.1 != 3, ]

#Fitting a unidimensional graded response model
mygrm <- mirt(mydata[, 3:8], model = 1, itemtype = rep("graded", 6), SE = T, verbose = F)

#Model fit
M2(mygrm, type = "C2")

#Item fit with S-X^2 statistics
itemfit(mygrm, method = "S-X2")

#Specifying and estimating a Multiple Group model.

#Creating a variable indicating the gender
gender <- ifelse(mydata$A3.1 == 1, "Male", "Female")

# Unconstrained model
#Using no anchor items as no previous information about DIF for these items
unconstrainedmodel <- multipleGroup(mydata[, 3:8], model = "F1 = 1-6",
                            group = gender, itemtype = rep("graded", 6),
                            optimizer = "nlminb")

# Constrained model
constrainedmodel <- multipleGroup(mydata[, 3:8], model = "F1 = 1-6",
                          group = gender, itemtype = rep("graded", 6),
                          invariance = c("free_means", "free_var", colnames(mydata[, 3:8])),
                          optimizer = "nlminb")

#Compare the groups using likelihood ratio test
#Hypothesis Test: Likelihood ratio test to compare models
anova(constrainedmodel, unconstrainedmodel) # reducted (less parameters) vs full model (more parameters)

#Selecting between models
#Select the unconstrained if p-value<0.05
#If constrained is better – no DIF
#If unconstrained is better - DIF analysis with all items
coef(unconstrainedmodel)

#Identify potential DIF of item 1-6 using the unconstrained model
#Unconstrained model - adding the constraints one item by one item.
DIF(unconstrainedmodel, which.par = c("a1", "d1", "d2", "d3", "d4", "d5", "d6"), scheme = "add", items2test = 1:6, optimizer = "nlminb")

#Items 4,5 and 6 exhibit Differential Item Functioning in the unconstrained model

#When DIF identified, new model with accounting for DIF
#Estimating the multiple group model accounting for DIF with respect to gender
modelDIF <- multipleGroup(mydata[, 3:8], 1, group = gender, itemtype = rep("graded", 6), invariance = c("free_means",
                                                                   "free_var", colnames(mydata[, 3:8])[-c(4,5,6)]), SE = TRUE, verbose = F)

#Perform sensitive analysis per item to see the the impact of the DIF

#Plotting the item category response functions for DIF items
itemplot(modelDIF, 4, "trace")
itemplot(modelDIF, 5, "trace")
itemplot(modelDIF, 6, "trace")

#Plotting the item information functions for DIF items
itemplot(modelDIF, 4, "info")
itemplot(modelDIF, 5, "info")
itemplot(modelDIF, 6, "info")

#Plotting the difficulty of the test (expected score function in each of the groups)
plot(modelDIF, type = "score")

#Extracting measurement precision (test information function in each of the groups)
#Test information plot
plot(modelDIF, type="info")

#EXTRA

#Plotting item category response functions for all the items with the final model accounting for DIF
itemplot(modelDIF, 1, "trace")
itemplot(modelDIF, 2, "trace")
itemplot(modelDIF, 3, "trace")
itemplot(modelDIF, 4, "trace")
itemplot(modelDIF, 5, "trace")
itemplot(modelDIF, 6, "trace")

#Plotting item information curves for all the items with the final model accounting for DIF
itemplot(object = modelDIF, item = 1, type = "info")  # Item 1
itemplot(object = modelDIF, item = 2, type = "info")  # Item 2
itemplot(object = modelDIF, item = 3, type = "info")  # Item 3
itemplot(object = modelDIF, item = 4, type = "info")  # Item 4
itemplot(object = modelDIF, item = 5, type = "info")  # Item 5
itemplot(object = modelDIF, item = 6, type = "info")  # Item 6

#DESCRIPTIVES
  
#Descriptive statistics
summary(mydata)

# Selecting columns 3 to 8
selected_columns <- mydata[, 3:8]
selected_columns <- as.data.frame(lapply(selected_columns, as.numeric))

# Calculating standard deviation, median, and range for each item
item_stats <- selected_columns %>%
  summarise(
    SD = apply(., 2, sd),        # Standard deviation for each item
    Median = apply(., 2, median),# Median for each item
    Range = apply(., 2, function(x) diff(range(x))) # Range for each item
  )

# Printing the results
print(item_stats)

# Calculate skewness and kurtosis for each item
desc <- describe(mydata)
skewness <- desc$skew
kurtosis <- desc$kurt

# Print the skewness and kurtosis for each item
for (i in 2:length(columns_to_keep)) {
  cat("Item:", columns_to_keep[i], "\n")
  cat("Skewness:", skewness[i], "\n")
  cat("Kurtosis:", kurtosis[i], "\n\n")
}

# Grouping the data by gender
gender_grouped <- mydata %>%
  group_by(Gender = gender) %>%
  select(-RespondentID)  # Remove the RespondentID column for the analysis

# Calculating descriptive statistics for each item and the overall scale by gender
descriptive_stats <- gender_grouped %>%
  summarise(
    across(starts_with("A24."), ~ mean(.x, na.rm = TRUE), .names = "Mean_{.col}"),  # Mean for each item
    across(starts_with("A24."), ~ sd(.x, na.rm = TRUE), .names = "SD_{.col}"),       # Standard deviation for each item
    across(starts_with("A24."), ~ median(.x, na.rm = TRUE), .names = "Median_{.col}"), # Median for each item
    across(starts_with("A24."), ~ diff(range(.x, na.rm = TRUE)), .names = "Range_{.col}") # Range for each item
  )

# Printing the results
print(descriptive_stats)

# Gender distribution
gender_distribution <- table(data$A3.1)
print("Gender Distribution:")
print(gender_distribution)

# Age distribution
age_distribution <- summary(data$A3.2)
print("Age Distribution:")
print(age_distribution)

# Educational background distribution
edu_background_distribution <- table(data$A3.10)
print("Educational Background Distribution:")
print(edu_background_distribution)

# Religion distribution
religion_distribution <- table(data$A3.8)
print("Religion Distribution:")
print(religion_distribution)

# Country distribution
country_distribution <- table(data$Country)
print("Country Distribution:")
print(country_distribution)

# Country and gender distribution
country_gender_distribution <- table(interaction(data$Country, ifelse(data$A3.1 == 1, "Male", "Female")))
print("Country and Gender Distribution:")
print(country_gender_distribution)

# Generate item-wise frequency tables
for (col_name in columns_to_keep) {
  cat("Item:", col_name, "\n")
  print(table(mydata[[col_name]]))
  cat("\n")
}

table(gender)

# Define a function to convert the age codes to actual age ranges
convert_age <- function(age_code) {
  age_ranges <- c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65 - 74", "75 - 84", "85 or older")
  return(age_ranges[age_code])
}

# Apply the conversion function to create a vector of age ranges
ages <- sapply(data$A3.2, convert_age)

# Print the minimum, mean, and maximum age
cat("Minimum Age:", min(ages), "\n")
cat("Mean Age:", mean(ages), "\n")
cat("Maximum Age:", max(ages), "\n")