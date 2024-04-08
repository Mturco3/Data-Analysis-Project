#MICHELE TURCO, EDOARDO BROWN, GIULIO PRESAGHI, IRENE BENVENUTI

Data = read.csv2("./Dataset/bank_accounts_train.csv", 
                 header = T, 
                 sep = ",", 
                 colClasses = "character")

#DATA CLEANING, EDA

#1

str(Data)

Data$CLIENTNUM = NULL # We drop the first line since it is a pure identifier and it has nothing to do with our analysis

variables = colnames(Data) # With this line we have a vector of our variables

categorical_variables <- c("Gender", "Education_Level", "Marital_Status", "Card_Category")

numerical_variables <- setdiff(variables, categorical_variables) # Setdiff is a function that finds the difference between the two argument sets

for (var in numerical_variables) {
  Data[[var]] = as.numeric(Data[[var]])
}

for (var in categorical_variables) {
  Data[[var]] = as.factor(Data[[var]])
}

str(Data)

target_variable = "Closed_Account"
numerical_variables <- setdiff(numerical_variables, target_variable)

# Plots categorical variables

# Gender
x = table(Data$Gender)
r = round(x/nrow(Data)*100, 2)
s = paste( r, "%", sep = "")
pie(table(Data$Gender))
{pie(x = table(Data$Gender), 
     labels = s,
     edges = 10000, 
     radius = 1,
     init.angle = 90, 
     col = c(rgb(1,0,0, .5),
             rgb(0,0,1,0.5)),
     cex = 2)
  mtext("Gender", side = 3, cex = 2)
  legend("topright", 
         pch = 15, 
         col = c(rgb(1,0,0, .5),
                 rgb(0,0,1,0.5)),
         c("Female", "Male"), cex = 1.7,
         bty = "n")}

#Marital Status
x <- table(addNA(Data$Marital_Status))
r = round(x/nrow(Data)*100, 2)
s = paste( r, "%", sep = "")
pie(table(Data$Marital_Status))
{pie(x = table(Data$Marital_Status), 
     labels = s,
     edges = 10000, 
     radius = 1,
     init.angle = 90, 
     col = c(rgb(1,0,0,0.5),
             rgb(0,0,1,0.5),
             rgb(0,0,0.5,1),
             rgb(1,0.5,1,1)
             ),
     cex = 1)
  mtext("Marital Status", side = 3, cex = 1.5, line = 1)
  legend("topright", 
         pch = 15, 
         col = c(rgb(1,0,0, .5),
                 rgb(0,0,1,0.5),
                 rgb(0,0, .5,1),
                 rgb(1,0.5,1,1)
                 ),
         c("Married", "Single", "Divorced", "Unknown"), cex = 1,
         bty = "n")}

#Educational Level
education_table <- table(Data$Education_Level)
r <- round(education_table / nrow(Data) * 100, 2)
s <- paste(r, "%", sep = "")

pie(x = education_table, 
    labels = s,
    edges = 10000, 
    radius = 1,
    init.angle = 90, 
    col = c(rgb(1,0,0,0.5),
            rgb(0,0,1,0.5),
            rgb(0,0,0.5,1),
            rgb(0.5,0.5,0,1),
            rgb(0.3,0,0.5,0.8),
            rgb(1,0.8,0,0.5),
            rgb(1,0.5,1,1)),
    cex = 1)
mtext("Education Level", side = 3, cex = 1.5, line = 1) # side=3 is the top
legend("topleft", 
       pch = 15, 
       col = c(rgb(1,0,0,0.5),
               rgb(0,0,1,0.5),
               rgb(0,0,0.5,1),
               rgb(0.5,0.5,0,1),
               rgb(0.3,0,0.5,0.8),
               rgb(1,0.8,0,0.5),
               rgb(1,0.5,1,1)),
       legend = levels(Data$Education_Level), cex = 1,
       bty = "n")

# Card Category
card_table <- table(Data$Card_Category)
r <- round(card_table / nrow(Data) * 100, 2)
s <- paste(r, "%", sep = "")
s2 <- paste(levels(Data$Card_Category), s)

pie(x = card_table, 
    edges = 10000, 
    radius = 1,
    init.angle = 90, 
    col = c(rgb(1,0,0,0.5),
            rgb(0,0,1,0.5),
            rgb(0,0,0.5,1),
            rgb(0.5,0.5,0,1)
            ),
    cex = 1)
mtext("Card Category", side = 3, cex = 1.5, line = 1) # side=3 is the top
legend( x= -2.4, y = 1,
       pch = 15, 
       col = c(rgb(1,0,0,0.5),
               rgb(0,0,1,0.5),
               rgb(0,0,0.5,1),
               rgb(0.5,0.5,0,1)
               ),
       legend = s2, cex = 1,
       bty = "n")

# Graphical exploration of Numerical Variables
Numerical_Data = Data[numerical_variables]
(means_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = mean))
colMeans(Numerical_Data)
(median_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = median))
(sd_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = sd))

ncol(Numerical_Data)
par(mfrow = c(3,3), mar = c(2,4,4,1))

for(i in 1:length(numerical_variables)){
  hist(Numerical_Data[,i], freq = F, main = names(Numerical_Data)[i],col = rgb(.7,.7,.7), border = "white", xlab = "")
  abline(v = means_vec[i], lwd = 2)
  abline(v = median_vec[i], lwd = 2, col = rgb(.7,0,0))
  legend("top", c("Mean", "Median"), lwd = 2, col = c(1, rgb(.7,0,0)),cex = .8, bty = "n")
}

#Encode NaN values
#Rename missing values with NA notation
Data[Data == 'Unknown'] <- NA

# Now we try to understand where the missing values are

# Create a logical matrix where TRUE indicates a missing value
na_matrix <- is.na(Data)

# Sum the TRUE values by row to see how many NAs each row contains
na_counts_per_row <- rowSums(na_matrix)

# Count how many rows have at least one NA
rows_with_na <- sum(na_counts_per_row > 0)

# Print the result
print(rows_with_na)

# Since there is a significant number of rows with missing values, dropping them is not an
# optimal solution

# Count NAs in each column
na_counts_per_column <- colSums(is.na(Data))

# Print the result
print(na_counts_per_column)

# Missing values are only present in "Marital Status" and "Educational Level". 


#We decided to impute missing values with the mode of the other instances
#in the same column

getSimpleMode <- function(x) {
  # Use table() to count occurrences of each value, sort in decreasing order, and return the name of the first element
  tbl <- table(x)
  mode_value <- names(tbl[tbl == max(tbl)])[1]
  return(mode_value)
}


for (var in categorical_variables) {
  if (any(is.na(Data[[var]]))) {
    Data[[var]][is.na(Data[[var]])] <- getSimpleMode(Data[[var]])
  }
}

dev.off() # empty plot

str(Data)
na_matrix <- is.na(Data)
# Count how many rows have at least one NA
na_counts_per_row <- rowSums(na_matrix)
rows_with_na <- sum(na_counts_per_row > 0)

# Print the result
print(rows_with_na)

## --------------------------------------------- ##
# Correlation Matrix

# Compute the correlation matrix
cor_matrix <- cor(Numerical_Data, use = "complete.obs")  # 'use' handles missing values

corrplot(cor_matrix, method = "color", tl.srt = 45, tl.col = "black") # Using the library corrplot


# Correlation with target variable
correlations <- sapply(Numerical_Data, function(x) cor(x, Data$Closed_Account, use = "complete.obs"))

# Omit the target variable from the plot if it's included in the Data frame
correlations <- abs(correlations[names(correlations) != "target_var"])

# Plot
# Increase the size of the margins on the left side (side = 2)
par(mar = c(5, 8, 4, 2) + 0.1)  # Default is c(5, 4, 4, 2) + 0.1

# Create the barplot with larger font size for names
barplot(correlations, main="Correlation with Target Variable",
        horiz=TRUE, cex.names=0.7, las=2, col = "deepskyblue")

# Reset to default par settings if necessary
par(mar = c(5, 4, 4, 2) + 0.1)






# View the cleaned data
head(Data)

#2

# Print the structure of the dataset
str(Data)

# Summary statistics for numerical variables
summary(Data)

# Frequency table for categorical variables
categorical_vars <- c("Gender", "Education_Level", "Marital_Status", "Card_Category")
lapply(Data[categorical_vars], table)

# Selecting numerical variables for correlation analysis
numerical_vars <- c("Customer_Age", "Dependent_count", "Months_on_book", "Total_Relationship_Count", "Months_Inactive_12_mon", "Contacts_Count_12_mon", "Credit_Limit", "Total_Revolving_Bal", "Avg_Open_To_Buy", "Total_Amt_Chng_Q4_Q1", "Total_Trans_Amt", "Total_Trans_Ct", "Total_Ct_Chng_Q4_Q1", "Avg_Utilization_Ratio")
cor_matrix <- cor(Data[numerical_vars], use="complete.obs") # handling NA values

# Visualize the correlation matrix
corrplot(cor_matrix, method="circle")

## Logistic regression

set.seed(123)
id_train <- sample(1:nrow(Data), size = 0.75*nrow(Data), replace = F)
train_data <- Data[id_train,]
val_data <- Data[-id_train,]

# Response variable distribution in the train test
plot(train_data$Closed_Account, ylab = "Frequency")
table(train_data$Closed_Account)
prop.table(table(train_data$Closed_Account))

# Response variable distribution in the original data
plot(Data$Closed_Account, ylab = "Frequency")
table(Data$Closed_Account)
prop.table(table(Data$Closed_Account))

# Response variable distribution in the validation set
plot(val_data$Closed_Account, ylab = "Frequency")
table(val_data$Closed_Account)
prop.table(table(val_data$Closed_Account))

# Fit logistic regression model
model <- glm(Closed_Account ~ Income + Gender, family = binomial(link = "logit"), data = train_data)
# View model summary
summary(model)

baseline <- glm(Closed_Account ~ 1, family = "binomial", data = train_data)
anova(logit_fit0, model, test = "Chisq")

#Now let's see how much it is accurate using the validation set
val_data$predicted_probabilities <- predict(model, newdata = val_data, type="response")

# Assuming your threshold is 0.5 for classifying predictions as 1
val_data$predicted_class <- ifelse(val_data$predicted_probabilities > 0.5, 1, 0)


# Create the confusion matrix by comparing actual values to predicted classes
conf_matrix <- table(Actual = val_data$Closed_Account, Predicted = val_data$predicted_class)
print(conf_matrix)


#Now let's show the probabilities on some plots
ggplot(val_data, aes(x = Income, y = predictions, color = Gender)) + 
  geom_line() + 
  labs(title = "Probability of Account Closure by Income and Gender", y = "Probability of Closure", x = "Income") +
  scale_color_manual(values = c("blue", "red"))

#From the logistic regression lines we can infere that income has not a different effect on males and females.
#Instead we can see that the Gender difference has a significant effect on the probability of Account Closure.

#K-NN

library(class)

# Set a range for k
k_values <- 1:20
accuracy_scores <- numeric(length(k_values))

# Loop over k values
for (k in k_values) {
  set.seed(123) # for reproducibility
  knn_pred <- knn(train = train_data[, c("Total_Trans_Amt", "Total_Trans_Ct")],
                  test = val_data[, c("Total_Trans_Amt", "Total_Trans_Ct")],
                  cl = train_data$Closed_Account,
                  k = k)
  
  # Calculate accuracy
  accuracy_scores[k] <- sum(val_data$Closed_Account == knn_pred) / length(knn_pred)
}

# Now, plot the accuracy scores as a function of k
plot(k_values, accuracy_scores, type = "b", 
     xlab = "Number of Neighbors (k)", ylab = "Accuracy",
     main = "k-NN Model Accuracy by Number of Neighbors")




