install.packages("rstatix")#통계테스트
install.packages("skimr")#데이터 흐름 요약

library(rstatix)
library(skimr)

#데이터 불러오기
df <- read.csv("C:/Users/USER/Desktop/02.다중회귀분석_실습/02.diabetes.csv")
skim(df)

#결측값 제거
df <- na.omit(df)

#불필요 변수 제거
df <- select(df, -c(Outcome))
df <- select(df, -c(var1, var2, var3))
skim(df)

#다중회귀분석
result <- lm(df$Diabetes ~ ., data = df)
result1 <- lm(df$Diabetes ~ 1, data = df) #다른 변수 고려 x(단순 평균으로 예측)

# ANOVA분석을 활용해 두 집단의 의미있는 차이가 있는지 확인(잔차제곱합 차이의 검증)
anova_result <- anova(result1, result)
print(anova_result)

#회귀모델
summary(result)

#tvalue 낮은거 제거
df <- select(df, -c(Pregnancies, BloodPressure, BMI, Age))
summary(result)

saveRDS(result, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")

new_data <- read.csv("C:/Users/USER/Desktop/02.다중회귀분석_실습/02.diabetes_test.csv")

predicted <- predict(loaded_model, newdata = new_data)
head(predicted)

results <- data.frame(new_data, Predicted = predicted)

#RMSE 계산
rmse <- sqrt(mean((results$Diabetes - results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))


#등분산성 : p>0.05보다 크면, 등분산성 가정을 유지 반대는 이분산성
library(lmtest)
bptest(result)

#독립성 : p>0.05보다 크면 잔차의 독립성을 가짐
library(car)
durbinWatsonTest(result)

#정규성 : p>0.05보다 크면 귀무가설 채택(정규성이 있음)
shapiro_test(result$residuals)

#4개의 조건에 대한 그래프를 모두 보는 방법
opar <- par(no.readonly = TRUE) # 그래프가 들어갈 Window 생성
par(mfrow = c(2, 2)) # 그래프가 들어갈 공간 생성 2X2
plot(result) # 그래프 결과 도출
par(opar) # 그래픽 매개변수 복원


#이상치검정 : cook's D 값이 높으면 문제
#세 기준에서(StudRes, Hat CookD) 상위 3개를 각각 뽑아 합친 것
cand <- influencePlot(result, id = list(n = 3))

mf <- model.frame(result) # 모형에 실제로 쓰인 데이터 (결측 제외된 상태)
z <- round(scale(mf[sapply(mf, is.numeric)]), 1)

rownames(z) <- rownames(mf) # 행 이름을 다시 붙임
cbind(round(cand, 2), z[rownames(cand), ])

out_rows <- c("229", "446") #확인 후 직접 입력
df2 <- df[!rownames(df) %in% out_rows, ]

#이상치가 제거된 데이터로 다시 분석 및 그래프 추출
result2 <- lm(Diabetes ~ ., data = df2)
cand <- influencePlot(result2, id = list(n = 3))
summary(result2)
