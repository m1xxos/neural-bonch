# Установка и подключение библиотеки
library(neuralnet)

# Функции преобразования
int_to_binvec <- function(n) {
  rev(as.integer(intToBits(n)[1:4]))
}

shift_left <- function(vec) {
  c(vec[-1], vec[1])
}

shift_right <- function(vec) {
  c(tail(vec, 1), head(vec, 3))
}

# Формирование обучающей выборки
data <- data.frame()
for (i in 1:15) {
  base <- int_to_binvec(i)
  left <- shift_left(base)
  right <- shift_right(base)
  # добавляем две строки: сдвиг влево (Y = 0), сдвиг вправо (Y = 1)
  data <- rbind(
    data,
    c(base, left, 0),
    c(base, right, 1)
  )
}

colnames(data) <- c(paste0("B", 1:4), paste0("S", 1:4), "Y")
data$Y <- as.numeric(data$Y)

# Обучение нейросети
set.seed(123)
nn <- neuralnet(
  Y ~ B1 + B2 + B3 + B4 + S1 + S2 + S3 + S4,
  data = data,
  hidden = c(8, 4),
  linear.output = FALSE,
  threshold = 0.01,
  stepmax = 1e6
)

# Предсказания
pred <- compute(nn, data[, 1:8])$net.result
pred_bin <- ifelse(pred > 0.5, 1, 0)

# Результаты
results <- cbind(data, Prediction = pred_bin)
print(results)

# Точность
accuracy <- mean(results$Y == results$Prediction)
cat(sprintf("\nТочность: %.2f%%\n", accuracy * 100))

plot(nn)

