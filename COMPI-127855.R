#Importing datasets
items = read.table(file="../data/ITEMS-127855.csv", header=TRUE) #importing dataset ITEMS-127855
head(items) #looking at the top of the dataset

students = read.table(file="../data/STUDENTS-127855.txt", header=TRUE, sep = ";") #importing dataset STUDENTS-127855
head(students) #looking at the top of the dataset

#Ordering the columns of the dataset students
students <- students[, c("ID", "REGION", paste0("BP", 1:31), setdiff(names(students), c("ID", "REGION", paste0("BP", 1:31))))]

#Renaming the "BP" columns to "ITEM" in students
for (i in 1:33) {
  name_old <- paste0("BP", i)
  name_new <- paste0("Item", i)
  colnames(students)[colnames(students) == name_old] <- name_new
}

#Renaming the domain columns in the dataset items
names(items)[2:5] <- c("D1", "D2", "D3", "D4")

#Converting all the Item variables to numeric
students[, paste0("Item", 1:31)] <- lapply(students[, paste0("Item", 1:31)], as.numeric)

#Renaming the Item variable to ITEM, in the dataset items
names(items)[1] <- "ITEM"

#Checking if there is any items that take a value outside 1-32 for the variable ITEM in the dataset items
items_wrong <- items[items$ITEM < 1 | items$ITEM > 32, ]
print(items_wrong)
#There is no rows that take a value outside 1-32 for the items variable in the dataset items. No auditing needed.

#Checking if there is any duplicated items in the dataset items, as there should only be one for each number of items from 1-32
duplicated_item <- duplicated(items$ITEM)

#Displaying the duplicated values, if there is any
if (any(duplicated_item)) {
  print("Values duplicated in the variable ITEM in the dataset items:")
  print(items$ITEM[duplicated_item])
} else {
  print("Found no duplicated values in the variable ITEM in the dataset items")
}
#We then see that item 17 is duplicated in the dataset items, so we want to check if the value for D1-D4 are the same for both of the item 17 rows.
items_sub <- items[items$ITEM == 17, ]
#Displaying the the subset of the dataset items
print(items_sub)
#We then see that the values for all the variables are the same for both rows that have ITEM=17. We therefore can just remove one of them, so that we have no duplicated items.
items <- items[-18, , drop = FALSE]
#Now there is no duplicated values in the variable ITEM in the dataset items

#Now we want to check for not possible values of D1-D4 in the dataset items
loop_variables <- c("D1", "D2", "D3", "D4")

#Looping through the variables and printing the values that are unique
for (col_name in loop_variables) {
  values_unique <- unique(items[[col_name]])
  cat("Unique values in", col_name, ":\n")
  print(values_unique)
}
#We see that all of the variables D1, D2, D3 and D4 only take the value of 1 or 0, which is correct as they state if the item is in that domain or not (1=yes, 0=no). Thus no auditing needed here.

#Finding duplicated values for "ID" within the same "REGION" in the dataset students
duplicates <- students[duplicated(students[c("ID", "REGION")]) | duplicated(students[c("ID", "REGION")], fromLast = TRUE), ]
#Printing the rows that are duplicated
print(duplicates)

#Row number 2969, 3622 and 3630 in the dataset students has the same values for ID and REGION
#Making a loop that looks at the two rows above and below the three mentioned rows
#Then updating the ID value for the three mentioned rows so that they fit with the ID value for the two rows above and below
  id_1 <- function(students, row_num) {
    if (row_num > 2 && row_num < nrow(students) - 1) {
      idorg_1 <- students$ID[row_num - 2]
      idorg_2 <- students$ID[row_num - 1]
      idnew_1 <- students$ID[row_num + 1]
      idnew_2 <- students$ID[row_num + 2]
      
      #Checking if the ID value is not correct in comparison with the rows above and below
      if (idorg_1 >= idorg_2 || idnew_1 <= idnew_2) {
        idnew <- (idorg_2 + idnew_1) / 2
        students$ID[row_num] <- idnew
      }
    }
    return(students)
  }
#Checking the row numbers 
row_n<- c(2969, 3622, 3630)
#Looping through the specified rows
for (row_num in row_n) {
  students <- id_1(students, row_num)
}
#Checking if the duplicates are now gone, after the audit is done
    duplicates <- students[duplicated(students[c("ID", "REGION")]) | duplicated(students[c("ID", "REGION")], fromLast = TRUE), ]
    print(duplicates)
#We now get no duplicates
    
#Finding all rows that have values other than 1-7 for the variable REGION
table(students$REGION)
    
#We can see that there is one row that has REGION=22, two rows that have REGION=33, one row that has REGION=66 and one row that has REGION=77
#We want to audit the rows that have values for the variable REGION other than 1-7, as there should be only 7 regions (with region number 1 to 7) 
      #Defining the values to look for the variable REGION (the ones that are invalid, as stated before)
      replace_value <- c(22, 33, 66, 77)
    
    #Looping through the dataset students
    for (i in 1:length(students$REGION)) {
      if (students$REGION[i] %in% replace_value) {
        #Finding the two rows above and below the invalid values of REGION
        index <- max(1, i - 2):min(i + 2, nrow(students))
        
        #Replacing the value for REGION with the same value as the two rows above and below the invalid values has
        students$REGION[i] <- students$REGION[index]
      }
    }
    
    table(students$REGION)
    #Now there is no values outside 1-7 for the variable REGION
      
#Checking for any ID variables that should not exist in the dataset students
region_number <- 7
    
#Making a loop
    for (region in 1:region_number) {
      #Checking how many rows each REGION has
      count_region <- nrow(students[students$REGION == region, ])
      
      #Printing how many rows each REGION has
      cat("Number of rows for REGION =", region, ":", count_region, "\n")
      
      #Computing an upper limit for the ID variable based on the number of rows each REGION has
      lim_id <- count_region
      
      #Finding the invalid rows for each REGION
      invalid_row <- students[students$REGION == region & (students$ID < 1 | students$ID > lim_id), ]
      
      #Printing the invalid rows
      print(invalid_row)
    }
#We get no invalid values of ID for REGION 1,2,4,5,6 and 7, so no auditing needed there
#We find that when REGION=3 in students the value for ID is 692 in row 1960. That is not possible, as there is only 408 rows for region 3
#We therefore want to audit row number 1960 in students

#Looking at the two rows above and below row number 1960 in students
print_rows0 <- c(1958:1962)
print(students[print_rows0, ])
#We can now see that row 1960 should take the value 294 for the ID variable when REGION=3, so lets audit the row to adhere to this
students$ID[1960] <- 294
#After this audit there is no more invalid valued of ID for REGION=3 in students

#Now we want to check if the variables Item1-Item31 in the dataset students take any values other than 0 or 1, as they should only take these values
#Creating a vector of variable names for Item1-Item31
name_variable <- paste0("Item", 1:31)

#Checking for values not equal to 0 or 1 by looping through each item-variable of the dataset studens
for (var_name in name_variable) {
  #Using the 'subset' function to filter out the rows where the item-variables are not equal to 0 or 1
  values_not_valid <- subset(students, students[, var_name] != 0 & students[, var_name] != 1)
  
  #Checking for values not valid
  if (nrow(values_not_valid) > 0) {
    cat(paste("Values not valid in", var_name, ":\n"))
    print(values_not_valid)
  }
}
#There are multiple invalid values of the item-variables
#For row number 1553, 1081, 1484, 380 and 3293 Item1=0.75, which is not possible
#We want to audit these values of items in the mentioned rows.
#We want to change these values of 0.75 to NA, because we can not be sure if they were supposed to be 0 or 1.
students[, 3:33][students[, 3:33] == 0.75] <- NA

#Rescoring the responses in the item variables so that if a student ends their test with a string of wrong responses (at lest 3 zeros), these are all replaced with NA
  responses_rescored <- function(students) {
    for (i in 1:nrow(students)) {
      row <- students[i, ]
      items_last <- row[31:33]
      if (all(is.na(items_last) | items_last == 0)) {
        students[i, 31:33] <- NA
        #Checking in reverse order for supplementary sequential zeros
        for (j in 30:1) {
          if (is.na(row[j]) || row[j] == 0) {
            students[i, j] <- NA
          } else {
            break}}}}
    return(students)
  }
#Applying the responses_rescored function to the dataset students
students <- responses_rescored(students)

#Now we are reshaping the dataset students from wide to long format
longstudents <- reshape(
  data = students,
  direction = "long",
  idvar = "ID",
  varying = list(c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7", "Item8", "Item9", "Item10",
                   "Item11", "Item12", "Item13", "Item14", "Item15", "Item16", "Item17", "Item18", "Item19",
                   "Item20", "Item21", "Item22", "Item23", "Item24", "Item25", "Item26", "Item27", "Item28",
                   "Item29", "Item30", "Item31")),
  v.names = "Y",
  timevar = "ITEM",
  new.row.names = 1:(nrow(students) * 31)
)

#Resetting the names of the rows
row.names(longstudents) <- NULL

#Adding a column named MREGION to longstudents
#Computung the mean of Y for each region
averagey_r <- tapply(longstudents$Y, longstudents$REGION, mean, na.rm = TRUE)

#Adding the mean of Y to longstudents
longstudents$MREGION <- averagey_r[match(longstudents$REGION, names(averagey_r))]

#Sorting the longstudents by column "ID"
longstudents <- longstudents[order(longstudents$ID), ]

#Adding a column named MSTUDENT to longstudents
#Calculating the mean of Y for each unique combination of REGION and ID
longstudents$MSTUDENT <- ave(longstudents$Y, longstudents$ID, longstudents$REGION, FUN = function(x) mean(x, na.rm = TRUE))

#Ordering longstudents by "REGION," "ID," and "ITEM"
ordering_ls <- with(longstudents, order(REGION, ID, as.numeric(gsub("ITEM", "", ITEM))))
longstudents <- longstudents[ordering_ls, ]

#Merging longstudents with items
finaldataset <- merge(longstudents, items, by = "ITEM")

#Ordering finaldataset by ID, REGION and ITEM
finaldataset<- finaldataset[order(finaldataset$REGION, finaldataset$ID, finaldataset$ITEM), ]
ordering_fd <- with(finaldataset, order(REGION, ID, as.numeric(gsub("ITEM", "", ITEM))))
finaldataset <- finaldataset[ordering_fd, ]
  
#Now we want to add a new ID variable that uniquely identifies all the 5000 students
#We want to change the name of the original id-variable first, so that we can call the new id-variable ID
  names(finaldataset)[names(finaldataset) == "ID"] <- "oldID"

#Adding an id-variable to uniquely identify each of the 5000 students
#We already know that there is 820, 846, 408, 856, 913, 456 and 701 unique students for region 1-7 respectively
#We also know there is in total 5000 students, which means 5000 unique combinations of ID and REGION
  newID <- function(oldID, REGION) {
    if (REGION == 1) {
      return(oldID)
    } else if (REGION == 2) {
      return(oldID + 820)
    } else if (REGION == 3) {
      return(oldID + 820 + 846)
    } else if (REGION == 4) {
      return(oldID + 820 + 846 + 408)
    } else if (REGION == 5) {
      return(oldID + 820 + 846 + 408 + 856)
    } else if (REGION == 6) {
      return(oldID + 820 + 846 + 408 + 856 + 913)
    } else if (REGION == 7) {
      return(oldID + 820 + 846 + 408 + 856 + 913 + 456)
    } else {
      return(NA) 
    }
  }
#Creating the new variable called ID
finaldataset$ID <- mapply(newID, finaldataset$oldID, finaldataset$REGION)
#Removing the oldID variable from finaldataset
finaldataset <- finaldataset[ ,-2]
 
#Adding a SCORE variable that takes a random domain and calculates the average Y per student across the items for the randomly chosen domain
#Randomly sampling a domain from D1, D2, D3 and D4
  random_d <- sample(c("D1","D2","D3","D4"), 1)

#Finding the items connected to each of the domains
itemsincluded <- items$ITEM[items[random_d] == 1]

#Subsetting the dataset finaldataset, based on the items chosen
finaldatasub <- finaldataset[finaldataset$ITEM %in% itemsincluded, ]

#Creating a vector to store the scores in
total_y <- rep(NA, nrow(finaldataset))

#Computing the average Y for each student (unique value of ID)
idunique <- unique(finaldataset$ID)

for (student_id in idunique) {
  student_rows <- finaldatasub[finaldatasub$ID == student_id, ]
  total_y[finaldataset$ID == student_id] <- mean(student_rows$Y, na.rm = TRUE)
  
}
#Adding the new variable SCORE to our dataset finaldataset 
finaldataset$SCORE <- total_y
  
#Ordering the variables to our liking
finaldataset <- finaldataset[, c("ID", "REGION", "MREGION", "MSTUDENT", "ITEM", "D1", "D2", "D3", "D4", "Y", "SCORE")]
#Changing variable Y to an integer variable
finaldataset$Y <- as.integer(finaldataset$Y)

#Now we have our final dataset complete
str(finaldataset)

#Saving the dataset finaldataset as an RDS-file
saveRDS(finaldataset, file = "DATA-127855.RDS")