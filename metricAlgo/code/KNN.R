## Евклидово расстояние
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")

iris30 = iris[sample(c(1:150), 30, replace=FALSE), 3:5]

plot(iris30[, 1:2], pch = 21, bg = colors[iris30$Species],
     col = colors[iris30$Species])
xl <- iris30[, 1:3]


## Сортируем объекты согласно расстояния до объекта z
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  ## Создаём матрицу расстояний
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  ## Сортируем
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}


## Применяем метод kNN
kNN <- function(xl, z, k)
{
  ## Сортируем выборку согласно классифицируемого объекта
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  ## Получаем классы первых k соседей
  classes <- orderedXl[1:k, n + 1]
  ## Составляем таблицу встречаемости каждого класса
  22
  counts <- table(classes)
  ## Находим класс, который доминирует среди первых k соседей
  class <- names(which.max(counts))
  return (class)
}


## Рисуем выборку
colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col
     = colors[iris$Species])


## Классификация одного заданного объекта
for (ytmp in seq(0, 3, by=0.1)){
  for (xtmp in seq(0, 7, by=0.1)){
    
    z <- c(xtmp, ytmp)
    xl <- iris[, 3:5]
    class <- kNN(xl, z, k=6)
    points(z[1], z[2], pch = 1, col = colors[class])
  }
}
