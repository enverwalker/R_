# СМПР
## Метрические алгоритмы
1. ### [Метод kNN и kwNN](/metricAlgo)

## Байесовские алгоритмы
1. ### [Линии уровня](/bayesAlgo/) (**[shiny](https://enverwalker.shinyapps.io/normallines/)**)
1. ### [Наивный байесовский классификатор](/bayesAlgo/) (**[shiny](https://enverwalker.shinyapps.io/naivebayes/)**)
1. ### [Подстановочный (plug-in) алгоритм](/bayesAlgo/) (**[shiny](https://enverwalker.shinyapps.io/plug-in/)**)

## Линейные алгоритмы
1. ### [ADALINE и правило Хебба](/linearAlgo/) (**[shiny](https://enverwalker.shinyapps.io/adalinehebb/)**)

## Линейные алгоритмы
Пусть ![](http://latex.codecogs.com/svg.latex?X%20%3D%20%5Cmathbb%7BR%7D%5En)
и ![](http://latex.codecogs.com/svg.latex?Y%20%3D%20%5C%7B-1%3B&plus;1%5C%7D).  
Тогда алгоритм ![](http://latex.codecogs.com/svg.latex?a%28x%2Cw%29%3D%20%5Ctext%7Bsign%7Df%28x%2Cw%29%3D%5Ctext%7Bsign%7D%20%28%5Clangle%20w%2Cx%20%5Crangle-w_0%29%2Cw%20%5Cin%20%5Cmathbb%7BR%7D%5En) - это __линейный алгоритм классификации__.  
Если _f_>0, то алгоритм _a_ относит _x_ к классу +1, иначе к классу -1,  
где ![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2Cx%20%5Crangle%3D0) - этой __уравнение разделяющей поверхности__.  

Тогда другими словами, если x находится по одну сторону гиперплоскости с её направляющим вектором w, объект x относится к классу +1, в противном случае - к классу -1.  

Величина ![](http://latex.codecogs.com/svg.latex?M_i%28w%29%3Dy_i%5Clangle%20x_i%2Cw%20%5Crangle) есть __отступ__ объекта относительно алгоритма классификации.  
Если ![](http://latex.codecogs.com/svg.latex?M_i%28w%29%3C0), это значит, что алгоритм совершает на объекте ![](http://latex.codecogs.com/svg.latex?x_i) ошибку.  

![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28M%29)
– монотонно невозрастающая __функция потерь__, мажорирует пороговую функцию
![](http://latex.codecogs.com/svg.latex?%5BM%3C0%5D%20%5Cleq%20%5Cmathcal%7BL%7D%28M%29).  
Отсюда следующий вид __минимизации суммарных потерь__:  
![](http://latex.codecogs.com/svg.latex?%5Ctilde%7BQ%7D%28w%2CX%5E%5Cell%29%20%3D%20%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%28M_i%28w%29%29%5Crightarrow%20%5Cmin_w)  

__! Стоит задача__ подобрать оптимальный вектор параметров *w*, минимизирующий эмпирический риск: ![](http://latex.codecogs.com/svg.latex?Q%3A%3D%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%7BL%7D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)

В этом нам поможет __Метод стохастического градиента__, который представляет собой *итерационный процесс* движения вектора *w*  в противоположную вектора градиента ![](https://latex.codecogs.com/gif.latex?Q%27%28w%2C%20X%5El%29) сторону.  

Движение продолжается, пока вектор *w* не перестанет изменяться и/или функционал *Q* не стабилизируется.
Градиент вычисляется не на всех объектах обучающей выборки, а на случайном объекте (отсюда название - "стохастический"). 
В зависимости от функции потерь ![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28M%29), которая используется в функционале эмпирического риска *Q*, будут получаться разные линейные алгоритмы классификации.  

Описание шагов __Метода стохастического градиента__:

1) Нормализация признаков.  
Чувствительность метода __SG__ к масштабу измерения признаков, заставляет нас провести процедуру их нормализации следующим образом:  
![](http://latex.codecogs.com/svg.latex?x%5Ej%3A%3D%5Cfrac%7Bx%5Ej-x%5Ej_%5Ctext%7Bmin%7D%7D%7Bx%5Ej_%5Ctext%7Bmax%7D-x%5Ej_%5Ctext%7Bmin%7D%7D), где  ![](http://latex.codecogs.com/svg.latex?x%5Ej) – _j_-й признак.  

2) Инициализация весов вектора *w*.
Обычно она производится присваиванием весам случайных малых значений:
![](http://latex.codecogs.com/svg.latex?w_j%3A%3D%5Ctext%7Brandom%7D%28-%5Cfrac%7B1%7D%7B2n%7D%2C&plus;%5Cfrac%7B1%7D%7B2n%7D%29), где _n_ – количество признаков _x_.  

3) Вычисление оценки функционала эмпирического риска: 
![](http://latex.codecogs.com/svg.latex?Q%3A%3D%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%7BL%7D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)  

В *итерационном процессе* на каждом шаге вектор *w* сдвигается в направлении наиболее быстрого убывания Q.
*(противоположно вектору градиента)*  

Веса вектора *w* меняются так: ![](http://latex.codecogs.com/svg.latex?w%3A%3Dw-%5Ceta%20Q%27%28w%29)  
или так:  
![](http://latex.codecogs.com/svg.latex?w%3A%3Dw-%5Ceta%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%7BL%7D%27%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29x_iy_i), где  
![](http://latex.codecogs.com/svg.latex?%5Ceta%3E0) – __темп обучения__. Оптимально брать темп: ![](https://latex.codecogs.com/gif.latex?%5Ceta%20%3D%20%5Cfrac%7B1%7D%7Biteration%7D)

4) Критерий останова заключается в оценке функционала эмпирического риска *Q* методом __экспоненциальной скользящей средней__:  
![](http://latex.codecogs.com/svg.latex?Q%3D%281-%5Clambda%29Q&plus;%5Clambda%20%5Cvarepsilon_i),  
где ![](http://latex.codecogs.com/svg.latex?%5Cvarepsilon_i%3D%5Cmathcal%7BL%7D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)
– __ошибка__ алгоритма на случайном элементе ![](http://latex.codecogs.com/svg.latex?x_i) из обучающей выборки,  
![](http://latex.codecogs.com/svg.latex?%5Clambda) – __параметр сглаживания__, он обычно равен: ![](http://latex.codecogs.com/svg.latex?%5Cfrac%7B1%7D%7B%5Cell%7D).  
*Если алгоритм не допустит ошибки ни на одном элементе или значение Q будет стабилизировано, то его работа будет остановлена*.  

### ADALINE  
В основе __ADALINE__(адаптивный линейный элемент) лежит __метод стохастического градиента__. Изменены только две вещи:  
1) Функция потерь имеет следующий вид:  
![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28M%29%3D%28M-1%29%5E2%3D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i-1%29%5E2)  
2) Веса обновляются по такому правилу:  
![](http://latex.codecogs.com/svg.latex?w%3Dw-%5Ceta%28%5Clangle%20w%2Cx_i%20%5Crangle-y_i%29x_i).  

Обучение ADALINE заключается в подборе наилучших значений вектора весов *w* с помощью функционала потерь. 
### Правило Хебба
Правило Хебба также является линейным алгоритмом.
Алгоритм использует кусочно-линейную функцию потерь: ![](img/hebbloss.png),\
и правило Хебба для обновления весов: ![](img/hebbupd.png)
## Реализация на R
Так как для обоих алгоритмов будет использоваться метод стохастического
градиента, то вынесем отдельно функции потерь и обновления весов, а затем
подставим их в стохастический градиент.

Готовая реализация описанного алгоритма на **shiny** доступна по **[ссылке](https://enverwalker.shinyapps.io/adalinehebb/)**.
```R
# Квадратичная функция потерь для ADALINE
adaLoss <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  l <- (mi - 1)^2
  return(l)
}
# дельта правило обновления для ADALINE
adaUpd <- function(xi, yi, w, eta) {
  wx <- c(crossprod(w, xi))
  #ld <- 2 * (wx - yi) * xi
  ld <- (wx - yi) * xi
  nextW <- w - eta * ld
  return(nextW)
}

# Кусочно-линейную функцию потерь для Хебба
hebbLoss <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  return (max(-mi, 0))
}
# правило Хебба для весов
hebbUpd <- function(xi, yi, w, eta) {
  nextW <- w + eta * yi * xi
  return (nextW)
}

## Стохастический градиент
stgrad <- function(xl, eta = 1, lambda = 1/6, eps = 1e-5, loss, upd, ...) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- rep(0.5, n)
  
  Q <- 0
  Qprev <- Q
  
  # Начальное значение Q
  for (i in seq(l)) {
    xi <- xl[i, 1:n]
    yi <- xl[i, n+1]
    
    Q <- Q + loss(xi, yi, w)
  }
  
  iter <- 0
  repeat {
    # мало ли, бесконечный цикл может быть
    iter <- iter + 1
    if (iter > 1000) {
      break
    }
    
    mis <- array(dim = l)
    for (i in seq(l)) {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      
      mis[i] <- crossprod(w, xi) * yi
    }
    
    errorIndexes <- which(mis <= 0)
    if (length(errorIndexes) == 0) {
      break
    }
    
    i <- sample(errorIndexes, 1)
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    
    ex <- loss(xi, yi, w)
    
    w <- upd(xi, yi, w, eta)
    
    Q <- (1 - lambda) * Q + lambda * ex
    # достигли стабилизация Q
    if (abs(Q - Qprev) < eps) {
      break
    }
    Qprev <- Q
    
    drawLine(w, ...)
  }
  
  return(w)
}

```
