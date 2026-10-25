install.packages("rstatix") 
install.packages("skimr") 
library(rstatix)
library(skimr)

df <- read.csv("C:/Users/gimji/OneDrive/Desktop/02.diabetes.csv", header = TRUE, stringsAsFactors = TRUE)
skim(df)

df <- na.omit(df)

df <- select(df, -c(Outcome)) 
df <- select(df, -c(var1, var2, var3)) 
skim(df)




result <- lm(df$Diabetes ~ ., data = df)
result1 <- lm(df$Diabetes ~ 1, data = df) 

anova_result <- anova(result1, result)
print(anova_result)
result
summary(result)
df <- select(df, -c(BMI, Age, Pregnancies,BloodPressure))

summary(result)

saveRDS(result, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")
loaded_model





new_data <- read.csv("C:/Users/USER/Desktop/dd/02.diabetes_test.csv")

predicted<- predict(loaded_model, newdata = new_data)
head(predicted)
 
results <- data.frame(new_data, Predicted = predicted)



rmse <- sqrt(mean((results$Diabetes- results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))