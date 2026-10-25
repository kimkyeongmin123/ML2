df <- read.csv("C:/Users/gimji/OneDrive/Desktop/02.diabetes.csv", header = TRUE, na = ".")
result <- lm(df$Diabetes ~ ., data = df)

AIC(Akaike information criterion) 
library("MASS")

result_bk = lm(df$Diabetes ~., data=df)
Df_fit_bk =stepAIC(result_bk, direction= "backward", trace = T) 

Df_fit_st = stepAIC(result_bk, direction = "both", trace = T)

result_fw = lm(df$Diabetes ~ 1, data=df)
Df_fit_fk =stepAIC(result_fw, direction= "forward",scope = (df$Diabetes ~ Pregnancies + SkinThickness + Insulin + BMI + Age + 
                                                              Outcome) ,trace = T)

vif_values <- vif(result)
print(vif_values)
selected_predictors <- names(vif_values[vif_values > 7])
print(selected_predictors)