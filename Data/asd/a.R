df <- read.csv("C:/Users/USER/Desktop/02.다중회귀분석_실습/02.diabetes.csv", header = TRUE, na = ".")
result <- lm(df$Diabetes ~ ., data = df)

AIC(Akaike information criterion) 
library("MASS")
#후진소거법
result_bk = lm(df$Diabetes ~., data=df)
Df_fit_bk =stepAIC(result_bk, direction= "backward", trace = T) #trace 하나씩 빼는 과정을 보여줌
#단계적선택법
Df_fit_st = stepAIC(result_bk, direction = "both", trace = T)
#전진선택법
result_fw = lm(df$Diabetes ~ 1, data=df)
Df_fit_fk =stepAIC(result_fw, direction= "forward",scope = (df$Diabetes ~ Pregnancies + SkinThickness + Insulin + BMI + Age + 
                                                              Outcome) ,trace = T)

vif_values <- vif(result)
print(vif_values)
selected_predictors <- names(vif_values[vif_values > 7])
print(selected_predictors)