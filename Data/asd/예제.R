install.packages("rstatix")
install.packages("skimr")

library(rstatix)
library(skimr)


df <- read.csv("C:/Users/gimji/OneDrive/Desktop/02.diabetes.csv")
skim(df)


df <- na.omit(df)


df <- select(df, -c(Outcome))
df <- select(df, -c(var1, var2, var3))
skim(df)


result <- lm(df$Diabetes ~ ., data = df)
result1 <- lm(df$Diabetes ~ 1, data = df)


anova_result <- anova(result1, result)
print(anova_result)


summary(result)
df

df <- select(df, -c(Pregnancies, BloodPressure, BMI, Age))
summary(result)

saveRDS(result, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")

new_data <- read.csv("C:/Users/gimji/OneDrive/Desktop/02.diabetes_test.csv")

predicted <- predict(loaded_model, newdata = new_data)
head(predicted)

results <- data.frame(new_data, Predicted = predicted)


rmse <- sqrt(mean((results$Diabetes - results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))



library(lmtest)
bptest(result)


library(car)
durbinWatsonTest(result)


shapiro_test(result$residuals)


opar <- par(no.readonly = TRUE) 
par(mfrow = c(2, 2)) 
plot(result) 
par(opar) 



cand <- influencePlot(result, id = list(n = 3))

mf <- model.frame(result) 
z <- round(scale(mf[sapply(mf, is.numeric)]), 1)

rownames(z) <- rownames(mf) 
cbind(round(cand, 2), z[rownames(cand), ])

out_rows <- c("229", "446") 
df2 <- df[!rownames(df) %in% out_rows, ]


result2 <- lm(Diabetes ~ ., data = df2)
cand <- influencePlot(result2, id = list(n = 3))
summary(result2)
