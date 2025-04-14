library(nnet)

# Загрузка данных iris во фрейм данных
data <- read.csv("C:/Users/bulyn/Documents/neural-bonch/column_3C.dat", header = FALSE, sep = " ")

# Исключаем столбец классов, сохраняем признаки отдельно
data$V7 <- NULL

# Формирование выходных реакций в виде трехкомпонентного вектора (DH, SL, NO)
targets <- class.ind(c(rep('DH', 100), rep('SL', 140), rep('NO', 70)))

# Формирование номеров для тренировочного набора
set.seed(55)
samp <- c(sample(1:100, 70), sample(101:240, 100), sample(241:310, 50))

# Формирование обучающего и тестового наборов
train.set <- data[samp, ]
test.set <- data[-samp, ]

# Обучение сети
set.seed(55)
net.model <- nnet(train.set, targets[samp, ], size = 2, maxit = 500)

# Предсказание на тестовом наборе
net.pred <- predict(net.model, test.set)

# Проверка точности предсказания
table(max.col(targets[-samp, ]), max.col(net.pred))

predicted.labels <- c("DH", "NO", "SL")[max.col(net.pred)]
actual.labels <- c("DH", "NO", "SL")[max.col(targets[-samp, ])]

# Таблица сравнения с реальными метками
cat("\nТаблица с реальными метками классов:\n")
print(table(Actual = actual.labels, Predicted = predicted.labels))

