data = read.csv("NIJ_s_Recidivism_Challenge_Full_Dataset (5).csv")

# Assuming the dataset has been imported as 'data'

# Cross-tabulation for Race and Supervision_Risk_Score_First
risk_score_table <- table(data$Race, data$Supervision_Risk_Score_First)
print(risk_score_table)

# Chi-square test for independence between Race and Supervision_Risk_Score_First
risk_score_chi_square <- chisq.test(risk_score_table)
print(risk_score_chi_square)

# Cross-tabulation for Race and Supervision_Level_First
supervision_level_table <- table(data$Race, data$Supervision_Level_First)
print(supervision_level_table)

# Chi-square test for independence between Race and Supervision_Level_First
supervision_level_chi_square <- chisq.test(supervision_level_table)
print(supervision_level_chi_square)


#Based on the given outputs, there are significant racial differences in both the supervision risk scores and levels upon release:
#Supervision Risk Score: The p-value for the Chi-square test between race and supervision risk score is less than 2.2e-16, which is much smaller than the significance level of 0.05. Therefore, you can reject the null hypothesis and conclude that there is a significant relationship between race and supervision risk score, indicating racial differences in the supervision risk scores upon release.
#Supervision Level: The p-value for the Chi-square test between race and supervision level is 3.569e-06, which is also smaller than the significance level of 0.05. This means you can reject the null hypothesis and conclude that there is a significant relationship between race and supervision level, indicating racial differences in the supervision levels upon release.
#In summary, the results show that there are significant racial differences in both the supervision risk scores and levels upon release.



#Based on the provided data, we can observe the following patterns:

#Supervision Risk Score:
#For lower risk scores (1, 2, 3), the proportion of WHITE individuals is higher than the proportion of BLACK individuals.
#For mid-range risk scores (4, 5, 6, 7), the proportion of BLACK individuals is higher than the proportion of WHITE individuals.
#For higher risk scores (8, 9, 10), the proportions are more balanced between the two racial groups, with the BLACK individuals still having a higher proportion in scores 8 and 9.
#Supervision Level:
#For the High supervision level, the proportion of BLACK individuals is higher than the proportion of WHITE individuals.
#For the Specialized supervision level, the proportion of BLACK individuals is higher than the proportion of WHITE individuals.
#For the Standard supervision level, the proportion of BLACK individuals is higher than the proportion of WHITE individuals.
#Based on these observations, there are differences in how race impacts supervision risk scores and levels upon release. The data shows that BLACK individuals have a higher proportion of higher risk scores and more restrictive supervision levels (High and Specialized). However, it's essential to remember that correlation does not imply causation, and these findings should be interpreted cautiously. It's important to consider other factors and contextual information when assessing racial disparities in supervision risk scores and levels upon release.



# Load the ggplot2 package for creating visualizations
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Normalize the risk_score_table to get proportions
risk_score_props <- prop.table(risk_score_table, margin = 1)

# Convert the risk_score_props matrix into a data frame for plotting
risk_score_df <- as.data.frame(risk_score_props)
risk_score_df$Race <- rownames(risk_score_props)[risk_score_df$Var1]
colnames(risk_score_df) <- c("Race_Index", "Risk_Score", "Proportion", "Race")

# Create a bar plot for supervision risk scores by race
ggplot(risk_score_df, aes(x = Risk_Score, y = Proportion, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Supervision Risk Scores by Race",
       x = "Supervision Risk Score",
       y = "Proportion") +
  theme_minimal()

# Normalize the supervision_level_table to get proportions
supervision_level_props <- prop.table(supervision_level_table, margin = 1)

# Convert the supervision_level_props matrix into a data frame for plotting
supervision_level_df <- as.data.frame(supervision_level_props)
supervision_level_df$Race <- rownames(supervision_level_props)[supervision_level_df$Var1]
colnames(supervision_level_df) <- c("Race_Index", "Supervision_Level", "Proportion", "Race")

# Create a bar plot for supervision levels by race
ggplot(supervision_level_df, aes(x = Supervision_Level, y = Proportion, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Supervision Levels by Race",
       x = "Supervision Level",
       y = "Proportion") +
  theme_minimal()


#--------------------------------------------------------------------------------------------------
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

library(shiny)
library(ggplot2)

# Normalize the risk_score_table to get proportions
risk_score_props <- prop.table(risk_score_table, margin = 1)

# Convert the risk_score_props matrix into a data frame for plotting
risk_score_df <- as.data.frame(risk_score_props)
risk_score_df$Race <- rownames(risk_score_props)[risk_score_df$Var1]
colnames(risk_score_df) <- c("Race_Index", "Risk_Score", "Proportion", "Race")

# Normalize the supervision_level_table to get proportions
supervision_level_props <- prop.table(supervision_level_table, margin = 1)

# Convert the supervision_level_props matrix into a data frame for plotting
supervision_level_df <- as.data.frame(supervision_level_props)
supervision_level_df$Race <- rownames(supervision_level_props)[supervision_level_df$Var1]
colnames(supervision_level_df) <- c("Race_Index", "Supervision_Level", "Proportion", "Race")

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Supervision Risk Scores and Levels by Race"),
  fluidRow(
    column(6,
           plotOutput("riskScorePlot")
    ),
    column(6,
           plotOutput("supervisionLevelPlot")
    )
  )
)

# Define the Shiny server
server <- function(input, output) {
  output$riskScorePlot <- renderPlot({
    ggplot(risk_score_df, aes(x = Risk_Score, y = Proportion, fill = Race)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Supervision Risk Scores by Race",
           x = "Supervision Risk Score",
           y = "Proportion") +
      theme_minimal()
  })
  
  output$supervisionLevelPlot <- renderPlot({
    ggplot(supervision_level_df, aes(x = Supervision_Level, y = Proportion, fill = Race)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Supervision Levels by Race",
           x = "Supervision Level",
           y = "Proportion") +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

#---------------------------------------------------------------------------------------------
#logistic regression model

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

library(dplyr)
library(car)

# Convert categorical variables into factors
data$Race <- as.factor(data$Race)
data$Age_at_Release <- as.factor(data$Age_at_Release)
data$Gang_Affiliated <- as.factor(data$Gang_Affiliated)
data$Education_Level <- as.factor(data$Education_Level)

# Create a binary variable for the risk score
threshold <- 5
data$High_Risk <- ifelse(data$Supervision_Risk_Score_First >= threshold, 1, 0)

# Perform the logistic regression
model <- glm(High_Risk ~ Race + Age_at_Release + Gang_Affiliated + Education_Level,
             family = binomial(link = "logit"),
             data = data)

# Display the summary of the logistic regression model
summary(model)

if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

library(caret)

# Remove rows with missing values in the 'High_Risk' variable
data_clean <- data[!is.na(data$High_Risk), ]

# Split the cleaned data into training (80%) and testing (20%) datasets
set.seed(123) # Set seed for reproducibility
split <- createDataPartition(data_clean$High_Risk, p = 0.8, list = FALSE)
train_data <- data_clean[split, ]
test_data <- data_clean[-split, ]


# Fit the logistic regression model using the training dataset
model <- glm(High_Risk ~ Race + Age_at_Release + Gang_Affiliated + Education_Level,
             family = binomial(link = "logit"),
             data = train_data)

summary(model)
# Make predictions on the testing dataset
predicted_probs <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0) # Set a threshold of 0.5 to classify the predicted probabilities

# Create a confusion matrix
cm <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$High_Risk))

# Display the confusion matrix
print(cm)

#------------------------------------------------------------------------------------------------------------------
install.packages("glmnet")
library(glmnet)

data <- data[!is.na(data$High_Risk), ]
data$High_Risk <- as.factor(data$Supervision_Risk_Score_First >= 6)

# Exclude the variables you don't want to include in the matrix
data_for_lasso <- data[, !(names(data) %in% c("ID", "Supervision_Risk_Score_First", "Supervision_Level_First", "Recidivism_Within_3years"))]

# Identify factors with only one level
single_level_factors <- sapply(data_for_lasso, function(x) is.factor(x) && length(levels(x)) < 2)

# Remove these factors from the dataset
data_for_lasso <- data_for_lasso[, !single_level_factors]

# Create the model matrix
X <- model.matrix(High_Risk ~ . - 1, data = data_for_lasso)


# Create the outcome variable
Y <- data$High_Risk


