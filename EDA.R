## --------------------EDA------------------------- ##

install.packages("corrplot")
library(corrplot)

#First of all we want to gather all the numerical_variables in a correlation matrix
#so that we can gain an insight on their relations with each other.

cor_matrix <- cor(Data[numerical_variables], use="complete.obs")
corrplot(cor_matrix, method = "color", tl.srt = 45, tl.col = "black") 
#From the correlation matrix we can clearly see some relevant correlations that 
#make us think of a possible deletion. In particular we are searching for some
#variables that are highly correlated so that we can discard one of them.
#It is worth to considering the relation between "Months_on_Book" and "Customer_Age",
#"Total_Trans_Ct" and "Total_Trans_Amt", "Credit_Limit" and "Avg_Open_to_Buy". The most
#Relevant one is the relation between "Credit_Limit" and "Avg_Open_to_Buy".


#Now let's analyze the categorical variables and their relations
head(categorical_variables)

# GENDER
x = table(Data$Gender)
r = round(x/nrow(Data)*100, 2)
s = paste( r, "%", sep = "")
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

#For Gender we analyze some relations 

##Gender-Income
ggplot(Data, aes(x = Gender, y = Income, color = Gender)) +
  geom_boxplot() +
  xlab("Gender") +
  ylab("Income") +
  scale_color_manual(values = c("F" = "pink", "M" = "blue"))

##Gender-Customer_Age
ggplot(Data, aes(x = Gender, y = Customer_Age, color = Gender)) +
  geom_boxplot() +
  xlab("Gender") +
  ylab("Customer_Age") +
  scale_color_manual(values = c("F" = "pink", "M" = "blue"))

##Gender-Months_On_Book
ggplot(Data, aes(x = Gender, y = Months_on_book, fill = Gender)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.8) +  # Add black outline and reduce transparency
  scale_fill_manual(values = c("#FF9999", "#66CCFF", "#FFFF99", "#99FF99")) +  # Custom fill colors
  labs(
    title = "Distribution of Months on Book by Marital Status",
    x = "Marital Status",
    y = "Months on Book"
  )

##Gender-Closed_Account
#Let's adjust the dimensions of the legend
smaller_text_theme <- theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12),
        strip.text.x = element_text(size = 10))

ggplot(Data, aes(x = Gender, fill = as.factor(Closed_Account))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "#FFFF99"), labels = c("Account Open", "Account Closed"), name = "") +
  labs(title = "Proportion of Closed Account By Gender", x = "Gender", y = "Proportion") +
  smaller_text_theme

#MARITAL STATUS
x <- table(addNA(Data$Marital_Status))
r = round(x/nrow(Data)*100, 2)
s = paste( r, "%", sep = "")
{pie(x = table(Data$Marital_Status), 
     labels = s,
     edges = 10000, 
     radius = 1,
     init.angle = 90, 
     col = c(rgb(1,0,0,0.5),
             rgb(0,0,1,0.5),
             rgb(0,0,0.5,1)
     ),
     cex = 1)
  mtext("Marital Status", side = 3, cex = 1.5, line = 1)
  legend("topright", 
         pch = 15, 
         col = c(rgb(1,0,0, .5),
                 rgb(0,0,1,0.5),
                 rgb(0,0, .5,1)
         ),
         c("Married", "Single", "Divorced"), cex = 1,
         bty = "n")}

#For Marital_Status we analyze some relations with numerical variables

##Marital_Status-Months_On_Book
ggplot(Data, aes(x = Marital_Status, y = Months_on_book, fill = Marital_Status)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.8) +  # Add black outline and reduce transparency
  scale_fill_manual(values = c("#FF9999", "#66CCFF", "#FFFF99", "#99FF99")) +  # Custom fill colors
  labs(
    title = "Distribution of Months on Book by Marital Status",
    x = "Marital Status",
    y = "Months on Book"
  )

##Marital_Status-Credit_Limit
ggplot(Data, aes(x = Marital_Status, y = Credit_Limit, fill = Marital_Status)) +
  geom_bar(stat = "identity", width = 0.7) +  # Set width of the bars
  labs(
    title = "Bar Plot Example",  # Title of the plot
    x = "Marital_Status",  # Label for the x-axis
    y = "Credit_Limit"  # Label for the y-axis
  ) +
  theme_minimal() +  # Use minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue", "#FFFF99", "lightgreen"))  # Custom fill colors

##Marital_Status-Closed_Account

ggplot(Data, aes(x = Marital_Status, fill = as.factor(Closed_Account))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "#FFFF99"), labels = c("Account Open", "Account Closed"), name = "") +
  labs(title = "Proportion of Closed Accounts by Marital_Status", x = "Income Bin", y = "Proportion") +
  smaller_text_theme

#EDUCATION LEVEL
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
            rgb(1,0.8,0,0.5)),
    cex = 1)
mtext("Education Level", side = 3, cex = 1.5, line = 1) # side=3 is the top
legend("topleft", 
       pch = 15, 
       col = c(rgb(1,0,0,0.5),
               rgb(0,0,1,0.5),
               rgb(0,0,0.5,1),
               rgb(0.5,0.5,0,1),
               rgb(0.3,0,0.5,0.8),
               rgb(1,0.8,0,0.5)),
       legend = levels(Data$Education_Level), cex = 1,
       bty = "n")

#For Education_Level we analyze some relations with numerical variables

##Education_Level-Credit_Limit
ggplot(Data, aes(x = Education_Level, y = Credit_Limit, fill = Education_Level)) +
  geom_bar(stat = "identity", width = 0.7) +  # Set width of the bars
  labs(
    title = "Credit Limit for each Educational Level",  # Title of the plot
    x = "Education_Level",  # Label for the x-axis
    y = "Credit_Limit"  # Label for the y-axis
  ) +
  theme_minimal() +  # Use minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  ) +
  scale_fill_manual(values = c("lightpink", "lightblue", "#FFFF99", "lightgreen", "brown", "violet"))

##Education_Level-Closed_Account

ggplot(Data, aes(x = Education_Level, fill = as.factor(Closed_Account))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "#FFFF99"), labels = c("Account Open", "Account Closed"), name = "") +
  labs(title = "Proportion of Closed Accounts by Education_Level", x = "Education_Level", y = "Proportion") +
  smaller_text_theme

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

#For Card_Category we analyze some relations with numerical variables

##Card_Category-Customer_Age
ggplot(Data, aes(x = Card_Category, fill = as.factor(Closed_Account))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "#FFFF99"), labels = c("Account Open", "Account Closed"), name = "") +
  labs(title = "Proportion of Closed Accounts by Card_Category", x = "Card_Category", y = "Proportion") +
  smaller_text_theme
