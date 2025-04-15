library(neuralnet)

set.seed(42)

# Подготовка данных
inputs <- expand.grid(X1 = c(0, 1), X2 = c(0, 1), X3 = c(0, 1), X4 = c(0, 1))
inputs$Y <- with(inputs, as.numeric(!X1 & X2 & X3 & X4))

# Обучение нейросети
nn <- neuralnet(
  Y ~ X1 + X2 + X3 + X4,
  data = inputs,
  hidden = c(6, 4),           
  threshold = 0.001,          
  stepmax = 1e7,              
  linear.output = FALSE
)

# Предсказание
predictions <- compute(nn, inputs[, 1:4])$net.result
rounded_predictions <- ifelse(predictions > 0.5, 1, 0)
results <- cbind(inputs, Prediction = rounded_predictions)

# Печать
print(results)

# Проверка точности
accuracy <- sum(results$Y == results$Prediction) / nrow(results)
cat(sprintf("\nТочность: %.2f%%\n", accuracy * 100))

plot(nn)
