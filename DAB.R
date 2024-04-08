#edo
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

numerical_variables <- setdiff(numerical_variables, "Closed_Account")

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


categorical_vars <- c("Gender", "Education_Level", "Marital_Status", "Card_Category")  # Your categorical variables

for (var in categorical_vars) {
  if (any(is.na(Data[[var]]))) {
    Data[[var]][is.na(Data[[var]])] <- getSimpleMode(Data[[var]])
  }
}

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
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method="circle")

install.packages("ggplot2")
library(ggplot2)

#Visualizing Credit Limit across different Card Categories
ggplot(Data, aes(x="Card_Category", y="Credit_Limit")) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title="Credit Limit by Card Category", x="Card_Category", y="Credit_Limit")
    

#### b. Scatter Plots for Numerical Variable Relations

#Relationship between Total Trans Amt and Total Trans Ct
ggplot(Data, aes(x="Total_Trans_Amt", y="Total_Trans_Ct")) +
  geom_point(alpha=0.5) +  # Alpha for transparency if many points overlap
  theme_minimal() +
  labs(title="Transaction Amount vs. Transaction Count", x="Total_Transaction_Amount", y="Total_Transaction_Count")


#### c. Histograms for Variable Distributions

# Histogram of Customer Age
ggplot(Data, aes(x="Customer_Age")) + 
  geom_histogram(binwidth=5, fill="blue", color="black") +
  theme_minimal() +
  labs(title="Distribution of Customer Age", x="Customer_Age", y="Frequency")

#LOGISTIC REGRESSION, K-NN

# Use 70% of dataset as training set and 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(Data), replace = TRUE, prob = c(0.7, 0.3))
train <- Data[sample, ]
test <- Data[!sample, ]

# Fit logistic regression model
model <- glm(train$Closed_Account ~ train$Income + train$Gender, family = binomial(link = "logit"), data = train)
# View model summary
summary(model)

#K-NN

library(class)

# Initialize an empty vector to store scores
scores <- numeric()

# Vary k and evaluate model performance
for (k in 1:20) {
  # Fit k-NN model using training data
  knn_model <- knn(train[, c("Total_Trans_Amt", "Total_Trans_Ct")],
                   test[, c("Total_Trans_Amt", "Total_Trans_Ct")],
                   train$Closed_Account, k = k)
  
  # Calculate performance metric (e.g., accuracy)
  accuracy <- sum(knn_model == test$Closed_Account) / nrow(test)
  
  # Store the score
  scores <- c(scores, accuracy)
}

# Plot scores vs. k
ggplot(data.frame(k = 1:20, score = scores), aes(x = k, y = score)) +
  geom_line() +
  labs(x = "Number of Neighbors (k)", y = "Test Accuracy",
       title = "Test Accuracy for some k for k-NN")


