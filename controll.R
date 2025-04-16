library(jpeg)

# Задаём список файлов (10 изображений) – убедитесь, что в рабочей директории имеются файлы *.jpg
# Если у вас имена файлов не следуют как "img1.jpg", "img2.jpg" и т.д.,
# то можно воспользоваться list.files(pattern="\\.jpg$") и затем выбрать 10 нужных
file_list <- list.files(path = "C:/Users/bulyn/Documents/neural-bonch/images", pattern = "\\.jpg$", full.names = TRUE)
if(length(file_list) < 10){
  stop("В рабочей директории должно быть минимум 10 изображений в формате jpg")
}
file_list <- file_list[1:10]  # Обрабатываем первые 10 изображений

# Список значений k для приближений
k_list <- c(5, 20, 50, 250)

# Для каждого изображения будем на отдельном графическом устройстве выводить результаты
for (file in file_list) {
  
  cat("Обработка файла:", file, "\n")
  
  # Загружаем изображение
  im <- readJPEG(file)
  
  # Перевод в оттенки серого (взвешенное усреднение каналов)
  X <- 0.2989 * im[,,1] + 0.5870 * im[,,2] + 0.1140 * im[,,3]
  
  # Получаем размеры изображения
  m <- nrow(X)
  n <- ncol(X)
  original_size <- m * n  # число пикселей
  
  # SVD-разложение изображения
  X.svd <- svd(X)
  U <- X.svd$u
  D <- diag(X.svd$d)
  V <- X.svd$v
  d_vals <- X.svd$d
  
  # Визуализируем приближённые изображения
  # Открываем новое окно или PDF для каждого изображения (если нужно, можно сохранить результаты)
  # Здесь используется окна с 2x2 графиками для разных k
  par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
  for (k in k_list) {
    Uk <- U[, 1:k]
    Dk <- D[1:k, 1:k]
    Vk <- V[, 1:k]
    Xk <- Uk %*% Dk %*% t(Vk)
    # Ограничиваем значения в диапазоне [0,1]
    Xk <- pmin(pmax(Xk, 0), 1)
    
    # Строим изображение с заголовком k и информацией по сохранённой информации
    info_percent <- round(cumsum(d_vals)[k] / sum(d_vals) * 100, 1)
    title_text <- paste("k =", k, "\nСохранено:", info_percent, "%")
    plot(0:2, type = "n", xlab = "", ylab = "", axes = FALSE, main = title_text)
    rasterImage(Xk, 1, 0, 3, 2)
  }
  
  # Вывод в консоль оценок для выбранных значений k
  cat(sprintf("Изображение: %s, размер: %d пикселей\n", file, original_size))
  for (k in k_list) {
    compressed_size <- m * k + k + n * k  # размер для хранения U, D и V (без учёта округлений)
    saved_ratio <- round(original_size / compressed_size, 2)
    info <- round(cumsum(d_vals)[k] / sum(d_vals) * 100, 1)
    cat(sprintf("  k = %-3d | сохранено информации: %5.1f%% | размер: %d → %d | выигрыш: %sx\n",
                k, info, original_size, compressed_size, saved_ratio))
  }
  cat("\n-------------------------------------------------------\n\n")
  
  # Если требуется сохранить результаты, можно воспользоваться функцией dev.copy() или pdf()
}

