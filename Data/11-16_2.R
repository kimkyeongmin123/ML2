library(pROC)
library(e1071) # SVM 모델
library(caret)

# 데이터 불러오기
df <- read.csv("C:/Users/USER/Desktop/03.로지스틱회귀분석/UniversalBank.csv")


df


#종속변수에 대한 데이터 요소로 변환
df$Personal.Loan <- factor(df$Personal.Loan,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

#데이터 나누기 학습 80%, 테스트 20%
trainIndex <- createDataPartition(df$Personal.Loan, p = 0.8, list = FALSE)
train_data <- df[trainIndex,]
test_data <- df[-trainIndex,]


cost_value <- 1 # Cost(C)
gamma_value <- 0.1 # Gamma

# SVM모델 생성
svm_model <- svm(train_data$Personal.Loan ~ ., data = train_data,
                 type = 'C-classification', #Soft margin
                 kernel = 'polynomial', #다항커널
                 cost = cost_value,
                 gamma = gamma_value,
                 probability = TRUE) #확률값으로 도출

summary(svm_model)

predicted_probs <- predict(svm_model, test_data, probability = TRUE)
predicted_probs

predicted_probs <- attr(predicted_probs, "probabilities")[, "Yes"] #객체의 속성을 추출하는 함수


predicted_classes <- ifelse(predicted_probs > 0.5, "Yes", "No")
predicted_classes <- as.factor(predicted_classes)
predicted_classes


conf_matrix <- confusionMatrix(predicted_classes, test_data$Personal.Loan)
print(conf_matrix)

roc_curve <- roc(test_data$Personal.Loan, predicted_probs)
plot(roc_curve)
auc(roc_curve)
roc_curve
