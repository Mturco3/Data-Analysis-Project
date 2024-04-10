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



# Graphical exploration of Numerical Variables
Numerical_Data = Data[numerical_variables]
means_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = mean)
colMeans(Numerical_Data)
median_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = median)
sd_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = sd)

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

Numerical_Data = Data[numerical_variables]
target_variable_numeric = as.numeric(Data$Closed_Account)

# Correlation with target variable
correlations <- sapply(Numerical_Data, function(x) cor(x, target_variable_numeric, use = "complete.obs"))

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
ggplot(val_data, aes(x = Income, y = predicted_probabilities, color = Gender)) + 
  geom_line() + 
  labs(title = "Probability of Account Closure by Income and Gender", y = "Probability of Closure", x = "Income") +
  scale_color_manual(values = c("blue", "red"))

#From the logistic regression lines we can infere that income has not a different effect on males and females.
#Instead we can see that the Gender difference has a significant effect on the probability of Account Closure.

#K-NN

library(class)

# Set a range for k
k_values <- 1:25
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

# Best Model

logit_fit1 <- glm(Closed_Account ~ .,
                  family = "binomial",
                  data = train_data)
summary(logit_fit1)

# Comparison based on deviance test (Chi-square test)
logit_fit0 <- glm(Closed_Account ~ 1,
                  family = "binomial",
                  data = train_data)
anova(logit_fit0, logit_fit1, test = "Chisq") 
# We do reject the null hypothesis of equivalence between the two models.

# Can we do better?
# We have too many covariates, some of which are not significant. We could
# perform some type of variable selection to keep only the most important ones.

## Stepwise variable selection (based on AIC) ##

# Forward
logit_fit_aic1 <- step(glm(Closed_Account ~ 1,
                           family = "binomial",
                           data = train_data),
                       scope = formula(logit_fit1),
                       direction = "forward")

# Backward
logit_fit_aic2 <- step(logit_fit1,
                       direction = "backward") 

# Both directions
logit_fit_aic3 <- step(logit_fit1,
                       direction = "both")

sort(coefficients(logit_fit_aic1))
sort(coefficients(logit_fit_aic2))
sort(coefficients(logit_fit_aic3))


# Comparison based on deviance test (Chi-square test) for nested models
anova(logit_fit_aic1, logit_fit1, test = "Chisq") 
# We do not reject the null hypothesis of equivalence between the two models.


## Stepwise variable selection (based on BIC) ##
# Forward
logit_fit_bic1 <- step(glm(Closed_Account ~ 1,
                           family = "binomial",
                           data = train_data),
                       scope = formula(logit_fit1),
                       direction = "forward",
                       k = log(nrow(train_data)))

# Backward
logit_fit_bic2 <- step(logit_fit1,
                       direction = "backward",
                       k = log(nrow(train_data))) 

# Both directions
logit_fit_bic3 <- step(logit_fit1,
                       direction = "both",
                       k = log(nrow(train_data)))

sort(coefficients(logit_fit_bic1))
sort(coefficients(logit_fit_bic2))
sort(coefficients(logit_fit_bic3))
# The methods select the same variables
# BIC is stricter than AIC and favors simpler models.

# Comparison based on deviance test (Chi-square test)
anova(logit_fit_bic2, logit_fit1, test = "Chisq") 
# We do reject the null hypothesis of equivalence!

# ----------------- model evaluation
# Classic threshold
tt <- 0.5

str(train_data)

# We can use this threshold to turn estimated probabilities into labels
pred_aic <- as.factor(ifelse(logit_fit_aic1$fitted.values > tt, "yes", "no"))
pred_bic <- as.factor(ifelse(logit_fit_bic1$fitted.values > tt, "yes", "no"))

## Training set ##
# based on aic
True.positive = sum(pred_aic[which(train_data$Closed_Account == 1)] == "yes")
True.negative = sum(pred_aic[which(train_data$Closed_Account == 0)] == "no")
False.positive = sum(pred_aic[which(train_data$Closed_Account == 0)] == "yes")
False.negative = sum(pred_aic[which(train_data$Closed_Account == 1)] == "no")
Confusion.Matrix = matrix(c(True.positive, 
                            False.positive, 
                            False.negative, 
                            True.negative),
                          nrow = 2,ncol = 2)
row.names(Confusion.Matrix) = c("Actual Positive", "Actual Negative")
colnames(Confusion.Matrix) = c("Predicted Positive", "Predicted Negative")
Confusion.Matrix

(Sensitivity = True.positive/(True.positive + False.negative))
(Specificity = True.negative/(True.negative + False.positive))
(Accuracy = (True.positive + True.negative) / nrow(train_data))

# based on bic
True.positive = sum(pred_bic[which(train_data$Closed_Account == 1)] == "yes")
True.negative = sum(pred_bic[which(train_data$Closed_Account == 0)] == "no")
False.positive = sum(pred_bic[which(train_data$Closed_Account == 0)] == "yes")
False.negative = sum(pred_bic[which(train_data$Closed_Account == 1)] == "no")
Confusion.Matrix = matrix(c(True.positive, 
                            False.positive, 
                            False.negative, 
                            True.negative),
                          nrow = 2,ncol = 2)
row.names(Confusion.Matrix) = c("Actual Positive", "Actual Negative")
colnames(Confusion.Matrix) = c("Predicted Positive", "Predicted Negative")
Confusion.Matrix

(Sensitivity = True.positive/(True.positive + False.negative))
(Specificity = True.negative/(True.negative + False.positive))
(Accuracy = (True.positive + True.negative) / nrow(train_data))
# On the training set the model based on aic performs slightly better in terms 
# sensitivity.
# This makes sense, since it uses a larger set of covariates.


# To effectively choose the model, we should evaluate the performance on some 
# data that has not been used for training.

# Predictions for the observations in the validation set

# Assuming 'df' is your dataframe
val_data_2 <- val_data[, -c((ncol(val_data)-1):ncol(val_data))]

prob_out_aic <- predict(logit_fit_aic1,
                        newdata = val_data_2,
                        type = "response")
pred_out_aic <- as.factor(ifelse(prob_out_aic > tt, "yes", "no"))
prob_out_bic <- predict(logit_fit_bic1,
                        newdata = val_data$Closed_Account,
                        type = "response")
pred_out_bic <- as.factor(ifelse(prob_out_bic > tt, "yes", "no"))

# based on aic
True.positive = sum(pred_out_aic[which(val_data$Closed_Account == 1)] == "yes")
True.negative = sum(pred_out_aic[which(val_data$Closed_Account == 0)] == "no")
False.positive = sum(pred_out_aic[which(val_data$Closed_Account == 0)] == "yes")
False.negative = sum(pred_out_aic[which(val_data$Closed_Account == 1)] == "no")
Confusion.Matrix_aic = matrix(c(True.positive, 
                                False.positive, 
                                False.negative, 
                                True.negative),
                              nrow = 2,ncol = 2)
row.names(Confusion.Matrix_aic) = c("Actual Positive", "Actual Negative")
colnames(Confusion.Matrix_aic) = c("Predicted Positive", "Predicted Negative")
Confusion.Matrix_aic

(Sensitivity_aic = True.positive/(True.positive + False.negative))
(Specificity_aic = True.negative/(True.negative + False.positive))
(Accuracy_aic = (True.positive + True.negative) / nrow(val_data))

# based on bic
True.positive = sum(pred_out_bic[which(val_dat$subs_end == "yes")] == "yes")
True.negative = sum(pred_out_bic[which(val_dat$subs_end == "no")] == "no")
False.positive = sum(pred_out_bic[which(val_dat$subs_end == "no")] == "yes")
False.negative = sum(pred_out_bic[which(val_dat$subs_end == "yes")] == "no")
Confusion.Matrix_bic = matrix(c(True.positive, 
                                False.positive, 
                                False.negative, 
                                True.negative),
                              nrow = 2,ncol = 2)
row.names(Confusion.Matrix_bic) = c("Actual Positive", "Actual Negative")
colnames(Confusion.Matrix_bic) = c("Predicted Positive", "Predicted Negative")
Confusion.Matrix_bic

(Sensitivity_bic = True.positive/(True.positive + False.negative))
(Specificity_bic = True.negative/(True.negative + False.positive))
(Accuracy_bic = (True.positive + True.negative) / nrow(val_dat))
# Also on the validation set, the model chosen via AIC is slightly better in terms 
# terms of sensitivity.

# Important: changing the threshold affects the results. 
# Hence, the threshold should be tuned to find the best one for our purpose (we 
# care more about the yes, more about the no, or equally about the two).


# ROC (Receiver Operating Characteristic) curve and AUC (Area Under the Curve)
# consider all the possible thresholds.
# AUC provides a single number that can be used to choose between models.
library(pROC)
?roc

## Training set ##
# ROC curves
roc_aic <- pROC::roc(train_dat$subs_end,
                     logit_fit_aic1$fitted.values,
                     plot = TRUE,
                     col = "midnightblue",
                     lwd = 3,
                     auc.polygon = T,
                     auc.polygon.col = "lightblue",
                     print.auc = T)
roc_bic <- pROC::roc(train_dat$subs_end,
                     logit_fit_bic1$fitted.values,
                     plot = TRUE,
                     col = "midnightblue",
                     lwd = 3,
                     auc.polygon = T,
                     auc.polygon.col = "lightblue",
                     print.auc = T)

# AUC scores
roc_aic$auc
roc_bic$auc
# The models seem to perform similarly on the training set in terms of 
# AUC


## Validation set ##
# ROC curves
roc_out_aic <- pROC::roc(val_dat$subs_end,
                         prob_out_aic,
                         plot = TRUE,
                         col = "midnightblue",
                         lwd = 3,
                         auc.polygon = T,
                         auc.polygon.col = "lightblue",
                         print.auc = T)
roc_out_bic <- pROC::roc(val_dat$subs_end,
                         prob_out_bic,
                         plot = TRUE,
                         col = "midnightblue",
                         lwd = 3,
                         auc.polygon = T,
                         auc.polygon.col = "lightblue",
                         print.auc = T)

# AUC scores
(auc_glm_aic <- roc_out_aic$auc)
(auc_glm_bic <- roc_out_bic$auc)
# The models perform similarly also on the validation set.

# Put the results toghether
(glm_aic <- c(Accuracy_aic, Sensitivity_aic, Specificity_aic, auc_glm_aic))
(glm_bic <- c(Accuracy_bic, Sensitivity_bic, Specificity_bic, auc_glm_bic))


## PCA 

# In order to pèerform a PCA, we have to consider only numerical variables

train_data_Numerical = train_data[numerical_variables]

# After it, we must scale the data

train_data_Numerical_Scaled= scale(train_data_Numerical)

# Performing PCA on the scaled data

pca <- princomp(train_data_Numerical_Scaled, cor = T, scale = F)

str(pca)

pca$loadings

val_data_Numerical = val_data[numerical_variables]
val_data_Numerical_Scaled= scale(val_data_Numerical)

# Predicting scores for the validation set using the PCA model
pca.val <- predict(pca, val_data_Numerical_Scaled)

# Subsetting the first 10 principal components
pca.val <- pca.val[, 1:10]

# Calculating the variance explained by each PCA component
pca.var <- pca$sdev^2
pca.var.percent <- pca.var / sum(pca.var)

# Calculating the cumulative variance explained
cum.pca.var.percent <- cumsum(pca.var.percent)

# Creating a data frame that stores the variance explained
pca.explained <- data.frame(
  Component = 1:length(pca.var.percent),
  Variance = pca.var.percent,
  CumulativeVariance = cum.pca.var.percent
)

# Printing the variance explained data frame
pca.explained


pca <- princomp(train_data_Numerical_Scaled, cor = T, scale = F) #already scaled

pca$loadings

val_data_Numerical = val_data[numerical_variables] # Validation data is scaled as the train data
val_data_Numerical_Scaled= scale(val_data_Numerical)

# Calculating the variance explained by each PCA component
pca_var <- pca$sdev^2
pca_var_percent <- pca_var / sum(pca_var)



library(RColorBrewer)

bar.comp = barplot(pca_var_percent,
                   las = 2,
                   col = rev(brewer.pal(9, "Blues")), 
                   border = F,
                   ylim = c(0, max(pca_var_percent)*1.5),
                   ylab = "Explained variance")

# choose components according to "elbow rule"

lines(x = bar.comp, pca_var_percent, type = "b", pch = 20, cex = 1.5, lwd = 4, col = 2)

text(bar.comp, 
     pca_var_percent + 3, 
     paste(round(pca_var_percent,1)),font = 2)

# Calculating the cumulative variance explained
cum_pca_var_percent <- cumsum(pca_var_percent)

# Predicting scores for the validation set using the PCA model
pca.val <- predict(pca, val_data_Numerical_Scaled)

# Subsetting the first 10 principal components
pca.val <- pca.val[, 1:10]
