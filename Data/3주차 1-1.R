#데이터 불러오기
train_data <- read.csv("C:/Users/USER/Desktop/dd/01.simple_train_data.csv")
#산점도
library(ggplot2)
ggplot(train_data, aes(x = thigh, y = upper_arm)) +
  geom_point(alpha = 0.7) +
  labs(title = "대퇴부 vs 상박부 산점도",
       x = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)") +
  theme_minimal()
#회귀분석 모델
model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)
#잔차
residuals(model)

