library(pROC)
library(caret)
library(e1071)

test_predictions <- predict(result, newdata = test_data, type = "response")
predicted_class <- ifelse(test_predictions > 0.5, "yes", "no")
predicted_class <- as.factor(predicted_class)
print(predicted_class)

confusion_matrix <- confusionMatrix(predicted_class, test_data$Personal.Loan)
confusion_matrix

roc_curve <- roc(test_data$Personal.Loan, test_predictions)
plot(roc_curve)
auc(roc_curve)
df_test <- read.csv("C:/Users/USER/Desktop/03.로지스틱회귀분석/UniversalBank_test.csv", header=TRUE)


