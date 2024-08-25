---
title: "Sesion 2 de R"
output: html_notebook
---

## Operaciones con Vectores

En R podemos crear listas con la funcion c(). Tambien podemos crear una secuencia con seq o ":"
```{r}
v1 = c(1, 3, 10, 5, 9)
v2 <- seq(1, 5)
v2 + v1
v1 * v2
v1 / v2
class(v1)
```
## Funciones sobre vectores
ejemplo: sumatoria de 
$$
\sum_{n=0}^{n=100} n
$$

```{r}
sum(v1)
#
nsize <- 100
milista <- 1:nsize
sum(milista)
#seq(from = 1, to = 1, by = ((to - #from)/(length.out - 1)),
#    length.out = NULL)
seq(3.1, 10.5, 5.0)
lista2 <- seq(3.1, 10.5, length.out = 7)
mean(lista2)
min(lista2)
max(lista2)
summary(lista2)
sort(lista2)
sqrt(lista2)
var(lista2)

names(lista2) <- c('a','b','c','d','e','f','g')
lista2["a"]

sd(lista2)
```
## indexacion y slicing en vectores
```{r}
letras <- c('a', 1,'c', 3.0,'e','f',c(1,2,3))
class(letras)
lista2[3]
lista2[1:3]
lista2[c(1, length(lista2))]
tail(lista2)
lista2[c("b", "d", "a")]
lista2.names
lista3 <- runif(10)
lista3[lista3 < 0.5]
lista3[lista3 != 0.5]
lista3[lista3 == lista3[1]]

lista3[lista3 == 0.8749436 ]

lista3 <= 0.6
lista3 >= 0.4
(lista3 >= 0.4) & (lista3 <= 0.6)
lista3[(lista3 >= 0.4) & (lista3 <= 0.6)]

```
# Matrices en R
```{r}
class(matrix(1:9, nrow = 3, ncol = 3))
matrix(1:16, nrow = 4, ncol = 4)
matrix1 <- matrix(runif(25), nrow = 5, ncol = 5)
matrix1[1,2:3] # un rango de elementos
matrix1[2,2] # un elemento
matrix1[2,] # una fila
matrix1[,4] # una una columna
matrix1[,c(3,5,1)]
colnames(matrix1) <- c("a","b","c","d","e")
matrix1
```
```{r}
lista_a = seq(1,5)
lista_b = seq(11,15)
matrix2 = matrix(c(lista_a, lista_b), nrow=2, ncol=length(lista_b))
matrix3 = cbind(lista_a, lista_b) # combinar por columna
class(matrix2)
matrix3/5

matrix3 == 1 
dim(matrix2)
matrix3 + t(matrix2)

matrix2 %*% matrix3 # producto matricial
matrix2 * t(matrix3) # elemento a elemento

dim(matrix2)
matrix(matrix2, nrow=length(matrix2), ncol=1)
```
## dataframes en R

Listo of available built-in datasets in R:
```
data()
```

```{r}
women
women$height
women$weight

df_sample <- data.frame(state.x77)
class(df_sample)
View(df_sample)  # muestra los datos en una pestaña aparte
df_sample$Life.Exp
summary(df_sample)
```

```{r}
df_sample[df_sample$Murder == max(df_sample$Murder), ]
df_sample$Population + df_sample$Income

df_sample$Population2 <- df_sample$Population
df_sample
```
## Crear un dataframe vacio
```{r}
df_sample2 <- data.frame()
c1 <- 1:20
c2 <- letters[1:20]
df_sample2 <- data.frame("nombre1" = c1, nombre2 = c2)
df_sample2
```






