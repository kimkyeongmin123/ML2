
install.packages("car")
library(car)
df <- read.csv("C:/Users/USER/Desktop/concrete_data.csv", header = TRUE)

result <- lm(Strength ~ ., data = df)

vif_values <- vif(result)

print(vif_values)
selected_predictors <- names(vif_values[vif_values > 7])
print(selected_predictors)


