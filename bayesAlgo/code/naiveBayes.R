naiveb = function(x, Py, Mo, covar, m, n) {
  kolvo <- matrix(c('setosa','versicolor', 'virginica', 0, 0, 0), nrow = 3, ncol = 2)
  schet = rep(0, m)
  for (i in 1:m) {
    schet[i] = Py[i]
    for (j in 1:n){
      N=1/sqrt(2*pi)/covar[i,j]*exp(-1/2*(x[j]-Mo[i,j])^2/covar[i,j]^2) #вычисление плотностей
      schet[i] = schet[i] * N #ищем для каждого класса
    }
    kolvo[i,2]=schet[i]
  }
  class <- kolvo[,1][which.max(kolvo[,2])]
}


xl <- iris[, 3:5]
n=2 #количество признаков
m=3 #количество классов
classes <- levels(xl[,3])
Py<-table(xl[,3])/dim(xl)[1] #априорная вероятность появления классов


Mo = matrix(0, nrow=m, ncol=n) #матрица мат. ожидания (для к-т центра выборки)
covar = matrix(0, nrow=m, ncol=n) #ковариационная матрица (отклонение)
for(i in 1:m){
  for(j in 1:n){
    temp=xl[xl[,3]==classes[i],][,j] 
    Mo[i,j]<-mean(temp) #мат. ожидание
    covar[i,j]<-sqrt(var(temp)) #формула коварицации
  }
}

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, xlab = "Petal Length", ylab = "Petal Width", main = "Наивный баесовский алгоритм ")

a=0
b=0
while(a<7){
  while(b<7){
    z <- c(b, a)
    class <- naiveb(z, Py, Mo, covar, m, n)
    points(z[1], z[2], pch = 21, col = colors[class], asp = 1)
    b=b+0.1
  }
  b=0
  a=a+0.1
}
