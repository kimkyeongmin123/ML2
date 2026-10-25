install.packages('rstatix')
install.packages('skimr')

library(rstatix)
library(skimr)
library(dplyr)
df <- read.csv('/Users/yuchan/Desktop/대학교 수업/R_code/머신러닝 2/Advertising.csv',
               header = TRUE, stringsAsFactors = TRUE)
skim(df)

df <- na.omit(df)
skim(df)

df <- select(df, -c(Outcome))
result <- lm(df$Sales ~ ., data = df) 
result1 <- lm(df$Sales ~ 1, data = df) 

anova_result <- anova(result1, result)
print(anova_result)

summary(result)

result <- update(result, . ~ . - Newspaper)
summary(result)




Fitted <- fitted(result)
Fitted
Residuals <- resid(result)
Residuals 

library(lmtest)
bptest(result)

library(car)
durbinWatsonTest(result)

shapiro_test(result$residuals)

opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(result)
par(opar)





install.packages('rstatix')
install.packages('skimr')

library(rstatix)
library(skimr)
library(dplyr)

library(lmtest)
bptest(result)

library(car)
vif_values <- vif(result)
print(vif_values)  


library("MASS")
result_bk = lm(df$Sales ~., data=df)
Df_fit_bk = stepAIC(result_bk, direction = 'backward', trace = T)


result_fw = lm(df$Sales ~ 1, data=df)
Df_fit_fk = stepAIC(result_fw, direction = 'forward',
                    scope = (Sales ~ TV + Radio), trace = T)




cand <- influencePlot(result, id = list(n = 3))

mf <- model.frame(result)
z <- round(scale(mf[sapply(mf, is.numeric)]), 1)

rownames(z) <- rownames(mf)
cbind(round(cand, 2), z[rownames(cand), ])

out_rows <- c('131', '6')
df2 <- df[!rownames(df) %in% out_rows, ]

result2 <- lm(Sales ~ ., data = df2)
cand <- influencePlot(result2, id = list(n = 3))

max(cooks.distance(result2))

summary(result2)




saveRDS(result2, 'regression_model.rds')
loaded_model <- readRDS('regression_model.rds')
