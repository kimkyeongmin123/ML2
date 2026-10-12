df <- read.csv("C:/Users/USER/Desktop/concrete_data.csv", header = TRUE)
result <- lm(df$Strength ~ ., data = df)

library(MASS)
#후진소거법
result_bk = lm(df$Strength ~., data=df)
Df_fit_bk =stepAIC(result_bk, direction= "backward", trace = T) #trace 하나씩 빼는 과정을 보여

#단계적선택법
Df_fit_st = stepAIC(result_bk, direction = "both", trace = T)
#전진선택법
result_fw = lm(df$Strength ~ 1, data=df)
Df_fit_fk =stepAIC(result_fw, direction= "forward",scope = (df$Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water + 
                                                              Superplasticizer + Coarse.Aggregate + Fine.Aggregate + Age
                                                            ),trace = T)
