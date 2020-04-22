X<-sample(c(1:150),replace=TRUE) #sample случайным образом переупорядочивает элементы, переданные в качестве первого аргумента
xl <- iris[X, 3:5]                  #replace=TRUE гарантирует, что элемент не будет дважды

#xl выборка по 150 случайным числам (от 1 до 150)

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") #цвета выборки

plot(xl[, 1:2], pch = 21, bg = colors[xl$Species], col= colors[xl$Species], asp = 1) #рисую выборку из 150 объектов

euclideanDistance <- function(u, v) #функция расстояния
{ 
  sqrt(sum(u - v)^2) 
}

nn <- function(z, xl)
{
  #print(xl) вывести выборку
  #определяю размерность выборки
  l <- nrow(xl)
  n <- ncol(xl)-1
  
  distances <- c() #вектор расстояний
  for (i in 1:l)
  {
    distances <- c(distances, euclideanDistance(xl[i, 1:n], z))
  }
  #print(distances) вывод расстояний
  #order(distances) упорядочивание расстояний
  xl[order(distances)[1], n+1]
}
#для всех точек определим класс и закрасим
for (ytemp in seq(0, 3, by=0.1)){
  for (xtemp in seq(0, 7, by=0.1)){
    z <- c(xtemp,ytemp) #ввод z
    class <- nn(z, xl) #берём класс из функции nn
    #print(nn(z,xl)) чтобы выводить словами класс
    points(z[1], z[2], pch = 21, col = colors[class], asp = 1) #закрашивание точек
  }
}

