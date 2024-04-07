#edo
#MICHELE TURCO, EDOARDO BROWN, GIULIO PRESAGHI, IRENE BENVENUTI

Data = read.csv2("C:/Users/Thebrickster21/Desktop/DataAnalProject/bank_accounts_train.csv", 
                 header = T, 
                 sep = ",", 
                 colClasses = "character")

#DATA CLEANING, EDA

#1

# Pre-processing
colnames(Data)

Data$CLIENTNUM = as.numeric(Data$CLIENTNUM)
Data$Customer_Age = as.numeric(Data$Customer_Age)
Data$Gender = as.factor(Data$Gender)
Data$Dependent_count = as.numeric(Data$Dependent_count)
Data$Education_Level = as.factor(Data$Education_Level)
Data$Marital_Status = as.factor(Data$Marital_Status)
Data$Card_Category = as.factor(Data$Card_Category)
Data$Months_on_book = as.numeric(Data$Months_on_book)
Data$Total_Relationship_Count = as.numeric(Data$Total_Relationship_Count)
Data$Months_Inactive_12_mon = as.numeric(Data$Months_Inactive_12_mon)
Data$Contacts_Count_12_mon = as.numeric(Data$Contacts_Count_12_mon)
Data$Credit_Limit = as.numeric(Data$Credit_Limit)
Data$Total_Revolving_Bal = as.numeric(Data$Total_Revolving_Bal)
Data$Avg_Open_To_Buy = as.numeric(Data$Avg_Open_To_Buy)
Data$Total_Amt_Chng_Q4_Q1 = as.numeric(Data$Total_Amt_Chng_Q4_Q1)
Data$Total_Trans_Amt = as.numeric(Data$Total_Trans_Amt)
Data$Total_Trans_Ct = as.numeric(Data$Total_Trans_Ct)
Data$Total_Ct_Chng_Q4_Q1 = as.numeric(Data$Total_Ct_Chng_Q4_Q1)
Data$Avg_Utilization_Ratio = as.numeric(Data$Avg_Utilization_Ratio)
Data$Income = as.numeric(Data$Income)
Data$Closed_Account = as.numeric(Data$Closed_Account)

#Encode NaN values
#Rename missing values with NA notation
Data[Data == 'Unknown'] <- NA

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


