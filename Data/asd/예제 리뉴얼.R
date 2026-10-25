install.packages('rstatix')
install.packages('skimr')

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

df <- select(df, -c(Pregnancies, BloodPressure, BMI, Age))

Fitted <- fitted(result) 
Residuals <- resid(result) 
library(ggplot2)
ggplot(df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted (선형성 확인)",
       x = "적합값 (Fitted Values)",
       y = "잔차 (Residuals)") +
  theme_minimal()

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

vif_values <- vif(result)
print(vif_values)
selected_predictors <- names(vif_values[vif_values > 7])
print(selected_predictors)


library("MASS")
result_bk = lm(df$Diabetes ~., data=df)
Df_fit_bk =stepAIC(result_bk, direction= "backward", trace = T) 

Df_fit_st = stepAIC(result_bk, direction = "both", trace = T)

result_fw = lm(df$Diabetes ~ 1, data=df)
Df_fit_fk =stepAIC(result_fw, direction= "forward",scope = (df$Diabetes ~ Glucose + SkinThickness + Insulin) ,trace = T)

saveRDS(result2, 'regression_model.rds')
loaded_model <- readRDS('regression_model.rds')


new_data <- read.csv("C:/Users/USER/Desktop/dd/02.diabetes_test.csv")

predicted<- predict(loaded_model, newdata = new_data)
head(predicted)

results <- data.frame(new_data, Predicted = predicted)



rmse <- sqrt(mean((results$Diabetes- results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))







