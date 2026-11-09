install.packages("GGally")
install.packages("tidyverse")
install.packages("tidymodels")
install.packages("rlang")
install.packages("skim")
install.packages("createDataPartition")
install.packages("broom")
install.packages("dplyr")
library(dplyr)
library(broom)
library(GGally) 
library(skimr)
library(tidyverse)
library(tidymodels)
library(caret)


df <- read.csv("C:/Users/USER/Desktop/03.로지스틱회귀분석/UniversalBank.csv", header = TRUE, na = ".")
skim(df)
df <- na.omit(df)
df
df <- select(df, -c(ID, ZIP.Code))
df$Personal.Loan <- ifelse(df$Personal.Loan == 1, "yes", "no")
df$Personal.Loan <- as.factor(df$Personal.Loan)

trainIndex <- createDataPartition(df$Personal.Loan, p = 0.8, list = FALSE)

train_data <- df[trainIndex,]
test_data <- df[-trainIndex,]

result1=glm(train_data$Personal.Loan ~1, data= train_data, family = "binomial")
result2=glm(train_data$Personal.Loan ~., data= train_data, family = "binomial")

anova(result1, result2, test = "Chisq")

summary(result2)
tidy_result <- tidy(result2)
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)
print(tidy_result)


library(car)

durbinWatsonTest(result2)

vif(result2)

result=glm(train_data$Personal.Loan ~., data= train_data, family = "binomial")

influencePlot(result, id.method="identify")

# 모델 저장
saveRDS(result, file = "logistic_model.rds")
# 모델 로드
Load <-readRDS("logistic_model.rds")
tidy_result <- tidy(Load)
tidy_result
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)
tidy_result
