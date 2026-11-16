library(caret)
library(broom)
library(dplyr)
library(car)
# 1. 데이터 다시 불러오기
df <- read.csv("C:/Users/USER/Desktop/air_quality_dataset.csv")

# 2. == 1 대신 중앙값(약 420.9)을 기준으로 "yes", "no" 변환
df$CO2 <- ifelse(df$CO2 > median(df$CO2), "yes", "no")
df$CO2 <- as.factor(df$CO2)

# 제대로 반반 나뉘었는지 확인 (no 250, yes 250이 나오면 성공!)
table(df$CO2)

# 3. 데이터 나누기 (학습 80%, 테스트 20%)

trainIndex <- createDataPartition(df$CO2, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data  <- df[-trainIndex, ]

# 4. 로지스틱 회귀 실행
result1 <- glm(CO2 ~ 1, data = train_data, family = "binomial")
result2 <- glm(CO2 ~ ., data = train_data, family = "binomial")

anova(result1, result2, test = "Chisq") # 모델 전체가 유의한가?
summary(result2)

tidy_result <- tidy(result2)
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)
print(tidy_result)

#독립성 검정
durbinWatsonTest(result2)
# 다중공선성(변수의 중복성) 
vif(result2)
# 다중공선성 위배 변수제거 후 다시 분석
result=glm(train_data$CO2 ~., data= train_data, family = "binomial") 
# Cook's distance 계산 (glm에도 사용 가능)
influencePlot(result, id.method="identify")

