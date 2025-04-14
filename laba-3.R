library(nnet)

# Загрузка данных iris во фрейм данных
data <- read.csv("C:/Users/bulyn/Documents/neural-bonch/column_3C.dat", header = FALSE, sep = " ")
head(data)

# Формирование выходных реакций в виде трехкомпонентного вектора
targets <- class.ind(c(rep('DH', 50), rep('SL', 50), rep('NO', 50)))

# Формирование номеров для тренировочного набора
set.seed(55)
samp <- c(sample(1:50, 35), sample(51:100, 35), sample(101:150, 35))

# Формирование обучающего и тестового наборов
train.set <- data.iris[samp, ]
test.set <- data.iris[-samp, ]

# Обучение сети
set.seed(55)
net.iris <- nnet(train.set, targets[samp, ], size = 2, maxit = 500)

# Предсказание на тестовом наборе
net.pred <- predict(net.iris, test.set)

# Проверка точности предсказания
table(max.col(targets[-samp, ]), max.col(net.pred))

