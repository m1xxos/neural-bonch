library(fpp2)
library(nnet)
library(NeuralNetTools)

# Загрузка данных для анализа
scripts <- elecequip
n <- length(scripts)  # n = 204 месяца (17 лет)

# Подготовка матрицы наблюдений заполненной нулями
LearnSeq <- matrix(rep(0, (n - 12) * 13), nrow = n - 12, ncol = 13)

# Заполнение матрицы данными для обучения
for (i in 1:(n - 12)) LearnSeq[i, ] <- scripts[i:(12 + i)]

# Обучение сети Nnet
set.seed(55)
Nnet <- nnet(LearnSeq[, 1:12], LearnSeq[, 13], size = 30,
             linout = TRUE, rang = 0.1, maxit = 300)

# Прогноз составленный обученной сетью Nnet
prognos <- c(rep(0, 12), Nnet$fitted.values)

# График фактического числа рецептов (blue) и предсказанных (yellow)
ggplot(scripts, aes(x = c(1:n))) +
  geom_line(aes(y = scripts), color = "blue", size = 2) +
  geom_line(aes(y = prognos), color = "yellow", size = 0.5)

plotnet(Nnet)
