library(GGally)
library(skimr)
library(tidyverse)
library(tidymodels)
library(caret)
library(car)
library(broom)
library(dplyr)
library(pROC)
library(e1071)
library(rpart) #CART모델
library(rpart.plot)

############################ 문제 1 ############################

# 데이터 불러오기
df <- read.csv("C:/Users/USER/Downloads/새 텍스트 문서.csv", header = TRUE, na = ".")
skim(df)

# 결측치 제거 (alkphos에 NA 4개)
df <- na.omit(df)

# 종속변수 : is_patient (1 = 환자, 2 = 비환자) → yes / no
df$is_patient <- ifelse(df$is_patient == 1, "yes", "no")
df$is_patient <- as.factor(df$is_patient)

# 데이터 나누기 80 / 20 (세 모델 모두 같은 데이터로 비교)
trainIndex <- createDataPartition(df$is_patient, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]


#1-1. 로지스틱 회귀분석 (변수 처리X)
result = glm(train_data$is_patient ~ ., data = train_data, family = "binomial")
summary(result)

test_predictions <- predict(result, newdata = test_data, type = "response")   # yes일 확률
predicted_class <- ifelse(test_predictions > 0.5, "yes", "no")
predicted_class <- as.factor(predicted_class)
print(predicted_class)

confusion_matrix <- confusionMatrix(predicted_class, test_data$is_patient)
print(confusion_matrix)

roc_curve <- roc(test_data$is_patient, test_predictions)
plot(roc_curve)
auc(roc_curve)


#1-2. SVM
cost_value <- 1     # Cost(C)
gamma_value <- 0.1  # Gamma
svm_model <- svm(train_data$is_patient ~ ., data = train_data,
                 type = "C-classification",   # Soft margin
                 kernel = "polynomial",       # 다항커널
                 cost = cost_value,
                 gamma = gamma_value,
                 probability = TRUE)          # 확률값으로 도출
summary(svm_model)

predicted_probs <- predict(svm_model, test_data, probability = TRUE)
predicted_probs <- attr(predicted_probs, "probabilities")[, "yes"]   # yes일 확률
predicted_classes <- ifelse(predicted_probs > 0.5, "yes", "no")
predicted_classes <- as.factor(predicted_classes)
predicted_classes

conf_matrix <- confusionMatrix(predicted_classes, test_data$is_patient)
print(conf_matrix)

roc_curve <- roc(test_data$is_patient, predicted_probs)
plot(roc_curve)
auc(roc_curve)


#1-3. Decision Tree
#Hyper parameters 설정
control_params <- rpart.control(cp = 0.01, maxdepth = 5, minsplit = 10)
#Complexity Parameter(분할될 때 마다 모델이 최소 1%는 개선이 되어야 함)
#maxdepth(트리의 깊이가 최대값), minsplit(각각의 노드에 최소 10개의 관측치가 있도록 분할)

#모델 생성
tree_model <- rpart(is_patient ~ ., data = train_data, method = "class", control = control_params)
summary(tree_model)
rpart.plot(tree_model)

# [,1] = 첫 번째 범주(no)일 확률 → 0.5 초과면 no
predicted_probs <- predict(tree_model, test_data, type = "prob")[,1]
predicted_probs
predicted_classes <- ifelse(predicted_probs > 0.5, "no", "yes")
predicted_classes <- as.factor(predicted_classes)
predicted_classes

confusion_matrix <- confusionMatrix(predicted_classes, test_data$is_patient)
print(confusion_matrix)

roc_curve <- roc(test_data$is_patient, predicted_probs)
plot(roc_curve)
auc(roc_curve)


############################ 문제 2 ############################

# 데이터 다시 불러오기
df <- read.csv("C:/Users/USER/Downloads/새 텍스트 문서.csv", header = TRUE, na = ".")
df <- na.omit(df)
df$is_patient <- ifelse(df$is_patient == 1, "yes", "no")
df$is_patient <- as.factor(df$is_patient)


#2-1. 로지스틱 회귀 → 신뢰수준 95%(p < 0.05)에서 유의한 변수 선정
result = glm(df$is_patient ~ ., data = df, family = "binomial")
summary(result)

tidy_result <- tidy(result)
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)
print(tidy_result)
# p < 0.05 : age, albumin, sgpt, sgot
# p > 0.05 → 제거 : gender, tot_bilirubin, direct_bilirubin, tot_proteins, ag_ratio, alkphos


#2-2. Cook's D > 0.5 데이터 제거 순서 바꿔야함 인플루언서를 제거하고 해야함
influencePlot(result, id.method = "identify")
max(cooks.distance(result))
# influencePlot 표에서 CookD > 0.5 인 행 번호를 적기 (167번 : CookD 0.815)
out_rows <- c("167")
df2 <- df[!rownames(df) %in% out_rows, ]

# 유의한 변수 + 종속변수만 남김
df2 <- select(df2, c(age, albumin, sgpt, sgot, is_patient))
skim(df2)


#2-3. Decision Tree
train_indices <- createDataPartition(df2$is_patient, p = 0.8, list = FALSE)
train_data <- df2[train_indices, ]
test_data <- df2[-train_indices, ]

control_params <- rpart.control(cp = 0.01, maxdepth = 5, minsplit = 10)
tree_model <- rpart(is_patient ~ ., data = train_data, method = "class", control = control_params)
summary(tree_model)
rpart.plot(tree_model)

predicted_probs <- predict(tree_model, test_data, type = "prob")[,1]   # no일 확률
predicted_probs
predicted_classes <- ifelse(predicted_probs > 0.5, "no", "yes")
predicted_classes <- as.factor(predicted_classes)
predicted_classes

confusion_matrix <- confusionMatrix(predicted_classes, test_data$is_patient)
print(confusion_matrix)

roc_curve <- roc(test_data$is_patient, predicted_probs)
plot(roc_curve)
auc(roc_curve)

