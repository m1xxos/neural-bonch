# --- Установка и подключение библиотек ---
# install.packages("deepnet")
# install.packages("readr")

library(deepnet)
library(readr)


train <- read_csv("neural-bonch/fashion-mnist_train.csv")
test <- read_csv("neural-bonch/fashion-mnist_test.csv")

# --- Предобработка данных ---
# Признаки: нормализация значений от 0 до 1
x_train <- as.matrix(train[, -1]) / 255
x_test <- as.matrix(test[, -1]) / 255

# Метки
y_train <- as.numeric(train[[1]])
y_test <- as.numeric(test[[1]])

# One-hot кодирование
one_hot <- function(labels, num_classes = 10) {
  m <- matrix(0, nrow = length(labels), ncol = num_classes)
  for (i in 1:length(labels)) {
    m[i, labels[i] + 1] <- 1
  }
  return(m)
}

y_train_oh <- one_hot(y_train)
y_test_oh <- one_hot(y_test)

# --- Обучение DBN ---
set.seed(123)
model <- dbn.dnn.train(
  x = x_train,
  y = y_train_oh,
  hidden = c(64, 16),         # Архитектура сети
  activationfun = "sigm",
  learningrate = 0.1,
  momentum = 0.5,
  learningrate_scale = 1.0,
  output = "softmax",
  numepochs = 10,
  batchsize = 100
)

# --- Оценка на тестовой выборке ---
predictions <- nn.predict(model, x_test)
predicted_labels <- max.col(predictions) - 1

accuracy <- mean(predicted_labels == y_test)
cat(sprintf("Точность на тестовой выборке: %.2f%%\n", accuracy * 100))


