train_data <- read.csv("C:/Users/USER/Desktop/단순회귀분석_데이터셋/01.simple_train_data.csv")

library(ggplot2)

#산점도
ggplot(train_data, aes(x = thigh, y = upper_arm)) + geom_point(alpha = 0.7) +
  labs(title = "대퇴부 vs 상박부 산점도", 
       x = "대퇴부 둘레 (thigh, cm)", 
       y = "상박부 둘레 (upper_arm, cm)") +
  
  theme_minimal()

#회귀분석 모델

model <- lm(upper_arm ~ thigh, data = train_data)
summary (model)




