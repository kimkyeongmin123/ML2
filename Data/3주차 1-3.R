train_data <- read.csv("C:/Users/USER/Downloads/ENB2012_data.csv")

library(ggplot2)

#산점도
ggplot(train_data, aes(x = X3, y = Y1)) + geom_point(alpha = 0.7) +
  labs(title = "벽면적 vs 난방 부하 산점도", 
       x = "벽면적 (X3, m^2)", 
       y = "난방 부하 (Y1, kcal/h)") +
  
  theme_minimal()

#회귀분석 모델

model <- lm(Y1 ~ X3, data = train_data)
summary (model)




