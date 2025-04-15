library(kohonen)

# Загрузка и подготовка данных
wine_data <- read.csv("C:/Users/bulyn/Documents/neural-bonch/winequality-red.csv", sep = ";")  
wine_input <- wine_data[, -12]                          
wine_scaled <- scale(wine_input)                         

set.seed(5)

# формирование SOM-карты: 1 строка, 25 колонок, топология hexagonal
sommap <- som(wine_scaled, grid = somgrid(10, 25, "hexagonal"))
# разбиение карты на отдельные кластеры

groups <- 6  # количество кластеров
som.hc <- cutree(hclust(dist(sommap$codes[[1]])), groups)

# построение SOM карты с раскраской по кластерам
plot(sommap, type = "codes", bgcol = rainbow(groups)[som.hc],
     main = "SOM карта вин (wines)")

# прорисовка границ кластеров
add.cluster.boundaries(sommap, som.hc)
