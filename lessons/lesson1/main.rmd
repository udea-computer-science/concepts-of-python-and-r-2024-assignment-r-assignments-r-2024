---
title: "Lesson R - 1"
output:
  html_notebook: default
  pdf_document: default
---

## Definir el directorio de trabajo
R necesita que se defina el directorio donde se esta trabajando, en jupyter notebook el directorio sera desde donde se ejecuta en el terminal el comando `Jupyter notebook`


```{r}
#Obtener el directorio de trabajo actual
working_dir <- getwd()
print(working_dir)
```

```{r}
dir.create('Datos')
## Definir otro directorio para trabajar
setwd(dir = './Datos') # ir a la carpeta Datos
getwd() #ver cual es el directorio actual de trabajo
```



```{r}
# regresar al directorio original de trabajo
setwd(working_dir)
getwd()
```

## Manejo de Paquetes
R tiene un repositorio donde estan la mayoria de [paquetes cran](https://cran.r-project.org/web/packages/available_packages_by_name.html):

- Desde el menu en Rstudio en: Packages->Install package(s).
- **Nota:** Anaconda instala los paquetes mas importantes para r asi:

Para importar librerias usamos `library`
```{r}
# Importar una libreria
library(stringr) # Esta ya esta instalada
```

Para averiguar el estado de instalacion de un paquete usamos `require`:
```{r}
# devuelve TRUE o FALSE según el paquete esté o no instalado
print(require(audio))
```

```{r}
library(audio) 
```



### Instalar, Eliminar y actualizar paquetes de CRAN


Para instalar paquetes se usa el comando `install.packages()`
```{r}
# Antes de instalar un paquete es bueno detectar si ya esta instalado
if(!require(audio)) # verificar si esta instalado el paquete
    install.packages("audio") # Comando para instalar paquetes, note las dobles comillas
library(audio) #Importar libreria
```

Para actualizar paquetes:
```{r}
# Actualizar paquetes de cran
update.packages() #se recomienda por linea de comando
```

Para remover paquetes temporalmente de la memoria:
```{r}
detach("package:audio", unload=TRUE)
```

Para eliminar definitivamente paquetes:
```{r}
remove.packages("audio") #es necesario las dobles comillas
```


Listar paquetes instalados:
```{r}
installed.packages()
```



Note que es dihelferente instalar un paquete a importarlo para hacerlo disponible en el kernel. La instalacion solo se
requiere una vez, mientras que la importacion se requiere cada que se va a usar:

```{r}
if(!require(sos)) # verificar el paquete 	Splines on the sphere (sos)
    install.packages("sos") # Comando para instalar paquetes, note las dobles comillas
library(sos) #Importar libreria
findFn("audio")# Buscar paquetes que contengan la palabra audio
```

### Instalar paquetes con conda
El listado de paquetes se puede ver en : https://anaconda.org/r/repo

Para instalar un paquete se hace en linea de comando asi:

`conda install -c r [nombre-del-paquete]`

Por ejemplo, para instalar el kernel de r en jupyter podemos usar conda:

```{bash}
#conda install -c r r-irkernel
#conda install -c r r-essentials
```


## Obteniendo ayuda con R
Además de una búsqueda en google o de visitar [StackOverflow](www.stackoverflow.com), hay algunas maneras incorporadas de obtener ayuda con R! La mayoría de las funciones R tienen documentación en línea.

- help(algo) documentacion
- help.search("algo") Busca en la ayuda
- apropos("algo") nombre de todos los objetos parecidos a ”algo”
- help.start() Inicia la version HTML de la ayuda

```{r}
# Necesita ayuda con vectors
help(vector)
```


```{r}
# Necesita ayuda con vectors
help(help.search)
```


Buscar en CRAN todo lo que contenga la palabra 'numeric':
```{r}
help.search('numeric')
```

```{r}
# Tambien se puede usar ??para buscar
??vector
```


## Aritmetica con R

```{r}
54 + 55
```

```{r}
55 - 50
```

```{r}
1/2
```

```{r}
2^2
```

```{r}
(1.5+1/2)^2
```

## Variables en R
```{r}
deposit <- 10
# Observe que parece una flecha apuntando hacia la variable.
variable.name <- 100
# Veamos la variable
variable.name
```




## Tipos de Datos
### Datos de punto flotante:
```{r}
n <- 2.2
print(n) # Jupyter notebook muestra las variables solo escribiendolas
```

### Datos enteros:
```{r}
i <- 5
print(i)
```

### Datos booleanos o Logicos:
```{r}
t <- TRUE
f <- FALSE
print(f)
```

### Caracteres (Characters)
los datos tipo Texto/string son conocidos como characters en R.
```{r}
# Comillas dobles
char <- "Data scientists, according to interviews and expert estimates, spend from 50 percent to 80 percent of their time mired in this more mundane labor of collecting and preparing unruly digital data, before it can be explored for useful nuggets"
char
```

```{r}
# Comillas simples
c <- 'Hola Mundo!'
c
```
### Verificar la typo de variable
Se puede realizar con la funcion class():

```{r}
class(t)
```


```{r}
v1 <- "Tom"
v2 <- "day"
str_interp("Hi ${v1}, Have a very nice ${v2} !")
```


```{r}
class(char)
```

Numericos
```{r}
class(n)
```

```{r}
class(Inf)
```



```{r}
class(NaN)
```


Complejos:

```{r}
typeof(4 + 5i)
```



### Manejo de Variables en el workspace
listado de las variables en el workspace
```{r}
ls()
```


Borrar una variable del workspace
```{r} 
rm(deposit)
ls()
```



Instalar libreria `tidyverse`
```{r}
install.packages("tidyverse")
library(tidyverse)

```


```{r}
data <- rnorm(1000)
hist(data)
# dev.off()
```

# Graficas en R

Grafica simple con funcion plot

```{r}
help(plot)
```

```{r}
plot(x=1:10, y=-1:-10, col=1:10, pch=19, main="oh hai dere")
```

Grafica de la funcion seno con la funcion plot
```{r}
x <- seq(0, 6 * pi, length.out = 100)
y <- sin(x)
plot(x, y, type = "b", col  = "red", lwd = 10)
```


## Liberria ggplot2
Es la principal libreria de graficos en R. Como importar la libreria:
```{r}
library(ggplot2)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
```


```{r}
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, alpha=Species)) + 
    geom_point(size=6, color="#69b3a2") +
    ggtitle("Transparency")
```

```{r}
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, shape=Species)) + 
    geom_point(size=6, color="lightblue") +
    ggtitle("Shape")
```

## Libraria Plotly
La libreria Plotly permite realizar graficos interactivos
```{r}
installed.packages("plotly")
```
```{r}
install.packages('plotly')
```



```{r}
library(plotly)

mtcars$am[which(mtcars$am == 0)] <- "Automatic"
mtcars$am[which(mtcars$am == 1)] <- "Manual"
mtcars$am <- as.factor(mtcars$am)

p <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c("#BF382A", "#0C4B8E")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Weight'),
                     yaxis = list(title = 'Gross horsepower'),
                     zaxis = list(title = '1/4 mile time')))

p
```



```{r}
require(grDevices) # for colours
x <- -6:16
op <- par(mfrow = c(2, 2))
contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, x, z)
contour(x, x, z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1,
        xlab = quote(x[1]), ylab = quote(x[2]))
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple",
        main = "20 levels; \"simple\" labelling method")
par(op)
```

