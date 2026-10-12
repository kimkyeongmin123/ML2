install.packages("rstatix") #통계 테스트
install.packages("skimr") #데이터의 흐름 요약
library(rstatix)
library(skimr)
#데이터 불러오기
df <- read.csv("C:/Users/USER/Desktop/concrete_data.csv", header = TRUE, stringsAsFactors = TRUE)
skim(df)

df <- na.omit(df)

skim(df)


result <- lm(df$Strength ~ ., data = df)
result1 <- lm(df$Strength ~ 1, data = df) # 다른 변수 고려 X(단순 평균으로 예측)

anova_result <- anova(result1, result)
print(anova_result)


#회귀모델
summary(result)
#모델 저장
saveRDS(result, "regression_model.rds")
#모델 불러오기
loaded_model <- readRDS("regression_model.rds")


Fitted <- fitted(result) # 적합값
Residuals <- resid(result) # 잔차
library(ggplot2)
ggplot(df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted (선형성 확인)",
       x = "적합값 (Fitted Values)",
       y = "잔차 (Residuals)") +
  theme_minimal()

StdResid <- sqrt(abs(df$Residuals)) # 표준화된 잔차의 크기 근사치
ggplot(df, aes(x = Fitted, y = StdResid)) +
geom_point(alpha = 0.6) +
geom_smooth(method = "loess", se = FALSE, color = "blue") +
labs(title = "Scale-Location Plot (등분산성 확인)",
x = "적합값 (Fitted Values)",
y = "Standardized Residuals") +
theme_minimal()