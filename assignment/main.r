###################   Serie 1 de la tarea, punto 1    ################

serie_1 <- function(n) {
    acum <- 0
    serie_acumulada <- c()
    for (i in 1:n){
        acum <- acum + (((-1)**i) * tanh(i))
        serie_acumulada <- c(serie_acumulada, acum)
        }
    return(serie_acumulada)
    }

n1_p1 <- 30
serie_punto_1 <- serie_1(n1_p1)
jpeg(file = "/workspaces/assignment-r-ederperez95/assignment/Serie_1_P1.jpeg")
plot(c(1:n1_p1), serie_punto_1, type = "l")
dev.off()
cat("\nSerie 1 de la tarea, punto 1")
cat("\nDebido a que la funcion 1 es periodica, no tiene convergencia\n")

###################   Serie 2 de la tarea, punto 1    ################

serie_2 <- function(n) {
    acum <- 0
    serie_acumulada <- c()
    for (i in 1:n){
        acum <- acum + (((atan(i))**2) / ((i**2) + 1))
        serie_acumulada <- c(serie_acumulada, acum)
        }
    return(serie_acumulada)
    }

n2_p1 <- 1000
serie_punto_2 <- serie_2(n2_p1)
jpeg(file = "/workspaces/assignment-r-ederperez95/assignment/Serie_2_P1.jpeg")
plot(c(1:n2_p1), serie_punto_2, type = "l")
dev.off()
cat("\nSerie 2 de la tarea, punto 1")
cat("\nLa funcion converge a:", serie_punto_2[length(serie_punto_2)])
cat("\n")

###################   Serie 3 de la tarea, punto 1    ################

serie_3 <- function(n) {
    acum <- 0
    serie_acumulada <- c()
    for (i in 2:n){
        acum <- acum + round((log(factorial(i), base = i)) / ((i)**3), 15)
        serie_acumulada <- c(serie_acumulada, acum)
        }
    return(serie_acumulada)
    }

n3_p1 <- 160
serie_punto_3 <- serie_3(n3_p1)
jpeg(file = "/workspaces/assignment-r-ederperez95/assignment/Serie_3_P1.jpeg")
plot(c(2:n3_p1), serie_punto_3, type = "l")
dev.off()
cat("\nSerie 3 de la tarea, punto 1")
cat("\nLa funcion converge a:", serie_punto_3[length(serie_punto_3)])
cat("\n")


###################   Serie 3 de la tarea, punto 1    ################

serie_seno <- function(x) {
    n <- 100
    acum <- 0
    radian <- pi / 180
    cons <- 2 * pi
    for (i in 0:n){
        valor <- (2 * i) + 1
        acum <- acum + ((((-1)**i) / factorial(valor)) * (((x * radian) %% cons)**valor))
        }
    return(acum)
    }

n1_p2 <- 360
valores_serie_seno <- sapply(c(0:n1_p2), serie_seno)
valores_serie_seno_r <- sapply(c(0:n1_p2 * (pi / 180)), sin)
serie_punto_3 <- serie_3(n3_p1)
jpeg(file = "/workspaces/assignment-r-ederperez95/assignment/Serie_Sen_P2.jpeg")
plot(c(0:n1_p2), valores_serie_seno, type = "l")
lines(c(0:n1_p2), valores_serie_seno_r, type = "l")
nombre_serie <- c("Funcion calculada a partir de la serie Seno", "Funcion Seno interna de R")
legend(x = "topright", legend = nombre_serie, lty = 1, col = 1:2)
dev.off()
cat("\nSerie 1 de la tarea, punto 2")
cat("\nLa suma de los valores de la funcion Seno calculada a partir de la serie es:", sum(valores_serie_seno))
cat("\nLa suma de los valores de la funcion Seno interna de R es:", sum(valores_serie_seno_r))
cat("\n")


###################   punto 3    ################
library(dplyr)
df <- read.csv("/workspaces/assignment-r-ederperez95/assignment/data.csv", header = FALSE, sep = "\t")
cat("\nLa suma de la segunda columna es:", sum(df["V2"]), "\n")
df$letras <- df["V1"]
agregacion_1 <- df %>% group_by(df["V1"]) %>% summarise(conteo_letras = count(letras))
cat("\nEl resultado del conteo de letras de la columna 1 es:\n")
print(agregacion_1)
agregacion_2 <- df %>% group_by(df["V1"]) %>% summarise(conteo_letras = sum(V2))
cat("\nLa suma de la columna 2 por las letras de la columna 1 es:\n")
print(agregacion_2)