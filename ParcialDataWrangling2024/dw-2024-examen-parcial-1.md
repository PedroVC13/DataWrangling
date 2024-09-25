dw-2024-parcial-1
================
Tepi
25/09/2024

# Examen parcial

Indicaciones generales:

- Usted tiene el período de la clase para resolver el examen parcial.

- La entrega del parcial, al igual que las tareas, es por medio de su
  cuenta de github, pegando el link en el portal de MiU.

- Pueden hacer uso del material del curso e internet (stackoverflow,
  etc.). Sin embargo, si encontramos algún indicio de copia, se anulará
  el exámen para los estudiantes involucrados. Por lo tanto, aconsejamos
  no compartir las agregaciones que generen.

## Sección 0: Preguntas de temas vistos en clase (20pts)

- Responda las siguientes preguntas de temas que fueron tocados en
  clase.

1.  ¿Qué es una ufunc y por qué debemos de utilizarlas cuando
    programamos trabajando datos?

Estas son funciones universales que basicamente son todas las funciones
elementales que se pueden aplicar a los arrays. Estas basicamente ayudan
para evitar tener que hacer bucles para poder operaciones dentro de los
arreys, por lo que también lo hace más rápido.

2.  Es una técnica en programación numérica que amplía los objetos que
    son de menor dimensión para que sean compatibles con los de mayor
    dimensión. Describa cuál es la técnica y de un breve ejemplo en R.

Esto se refiere al broadcasting, que lo que hace es que permite ampliar
arreys o variables de menor dimensión hasta donde pueda ser operada con
un arrey o una variable de una mayor dimensión

``` r
vector <- c(8, 4, 1)

matriz <- matrix(1:15, nrow=3)

broadcasting <- matriz * vector

print(broadcasting)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    8   32   56   80  104
    ## [2,]    8   20   32   44   56
    ## [3,]    3    6    9   12   15

3.  ¿Qué es el axioma de elegibilidad y por qué es útil al momento de
    hacer análisis de datos?

Este es un axioma que nos dice que siempre se va a elegir la opción que
genere un mayor beneficio, despues de haber evaluado todas las opciones.
En el analisis de datos es utila ya que estos lo que reflejaran son las
mejores opciones que pudieron elegir las personas, también al momento de
hacer algun modelo se puede suponer que escogerán la opción que tenga un
mayor beneficio.

4.  Cuál es la relación entre la granularidad y la agregación de datos?
    Mencione un breve ejemplo. Luego, exploque cuál es la granularidad o
    agregación requerida para poder generar un reporte como el
    siguiente:

| Pais | Usuarios |
|------|----------|
| US   | 1,934    |
| UK   | 2,133    |
| DE   | 1,234    |
| FR   | 4,332    |
| ROW  | 943      |

La granularidad es el nivel de detalle que tienen los datos, mientras
que la agregación es el poder resumir los datos más detallados a un
nivel más general o resumido.Entonces se puede decir que mientras uno
esta disminuyendo la granularidad los datos, de cierta forma estamos
haciendo agregación, ya que se estan intentando resumir de una forma en
la que se vean mas generales. En el reporte anterior se podría decir que
se está haciendo una agregación, ya que el numero de usuarios fueron
contados dependiendo de su país, para poder mostrar los datos mas
generales.

## Sección I: Preguntas teóricas. (50pts)

- Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
  deberá responder 5. Las 5 a responder estarán determinadas por un
  muestreo aleatorio basado en su número de carné.

- Ingrese su número de carné en `set.seed()` y corra el chunk de R para
  determinar cuáles preguntas debe responder.

``` r
set.seed(20220189) 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 1, 3, 7, 8, 10"

Preguntas: 1, 3, 7, 8, 10

### Listado de preguntas teóricas

1.  Para las siguientes sentencias de `base R`, liste su contraparte de
    `dplyr`:

    - `str()`
    - `df[,c("a","b")]`
    - `names(df)[4] <- "new_name"` donde la posición 4 corresponde a la
      variable `old_name`
    - `df[df$variable == "valor",]`

    \*``` glimpse()``  * ```df %\>% select(a, b)\``*`df %\>%
    rename(new_name = old_name)`*`df %\>% filter(variable == “valor”)\`

2.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

Esto se puede hacer debido a que es un lenguaje vectorizado, por lo que
se facilita el poder trabajar con arrays. Por lo que hace que sea un
código mas corto y que su implementación sea mas eficiente.

7.  ¿Qué pasa si quiero agregar una nueva categoría a un factor que no
    se encuentra en los niveles existentes?

No permite que esto se haga, y mostrará un error. Esto se debe a que no
se puede agregar la cetegoría solo así, primero se debería de agregar el
nivel para poder agergar la cetegoría, de lo contrario no se puede
hacer.

8.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?
    - El nuevo elemento
    - `NA`

El resultado será un NA, ya que no está entre nlos niveles existentes,
por lo que lo reconoce como un valor no valido y por lo tanto lo
sustituye con el NA.

10. Si quiero obtener como resultado las filas de la tabla A que no se
    encuentran en la tabla B, ¿cómo debería de completar la siguiente
    sentencia de SQL?

    - SELECT \* FROM A Left Join B ON A.KEY = B.KEY WHERE B.KEY IS NULL

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

## Sección II Preguntas prácticas. (30pts)

- Conteste las siguientes preguntas utilizando sus conocimientos de R.
  Adjunte el código que utilizó para llegar a sus conclusiones en un
  chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### I. Preguntas teóricas

## A

``` r
parcial_anonimo <- readRDS("~/Desktop/DataWrangling/ParcialDataWrangling2024/parcial_anonimo.rds")
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
head(parcial_anonimo)
```

    ##         DATE Codigo Material Descripcion     Pais Distribuidor Territorio
    ## 1 2018-12-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 2 2018-11-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 3 2018-10-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 4 2018-09-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 5 2018-08-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 6 2018-07-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ##    Cliente    Marca Canal Venta Unidades plaza  Venta
    ## 1 9d6e1d65 61d7fbfc    7b48292e              2  26.50
    ## 2 9d6e1d65 61d7fbfc    7b48292e              0   0.00
    ## 3 9d6e1d65 61d7fbfc    7b48292e              3  39.75
    ## 4 9d6e1d65 61d7fbfc    7b48292e              3  39.75
    ## 5 9d6e1d65 61d7fbfc    7b48292e              8 106.00
    ## 6 9d6e1d65 61d7fbfc    7b48292e              3  39.75

``` r
rentabilidad <- parcial_anonimo %>%
  group_by(Cliente) %>%
  summarise(Ventas = sum(Venta),Paises = n_distinct(Pais))%>%
  filter(Paises>1) %>%
  arrange(desc(Ventas))%>%
  head(3)

print(rentabilidad)
```

    ## # A tibble: 3 × 3
    ##   Cliente  Ventas Paises
    ##   <chr>     <dbl>  <int>
    ## 1 a17a7558 19818.      2
    ## 2 ff122c3f 15359.      2
    ## 3 c53868a0 13813.      2

Se puede ver que lo mas que logramos vender a distintos países por
cliente, son dos países. Y de esto podemos ver que el cliente que mas
rentable resulta para nosotros es el cliente a17a7558, tomando como
rentabilidad las ventas totales que este nos puede generar.Este cliente
nos genera ventas de 19,800 aproximadamente. Con una diferencia de
aproximadamente 4,000 respecto a nuestro segundo cliente mas rentable.

## B

``` r
library(dplyr)

perdidas <- parcial_anonimo %>%
  group_by(Territorio) %>%
  summarise(Perdidas = sum(Venta[Venta < 0], na.rm = TRUE),Ventas = sum(Venta[Venta>0], na.rm = TRUE), Ratio = ifelse(Perdidas == 0, NA, Ventas / abs(Perdidas)))%>%
  arrange(Ratio)%>%
  head(3)

print(perdidas)
```

    ## # A tibble: 3 × 4
    ##   Territorio Perdidas Ventas Ratio
    ##   <chr>         <dbl>  <dbl> <dbl>
    ## 1 68de9759      -226.  3930.  17.4
    ## 2 8682908b      -245.  4857.  19.8
    ## 3 45c0376d      -475. 11121.  23.4

Podemos ver que el ratio entre lo que hacen de ventas a comparación con
las perdidas en el territorio 68de9759 es menor, teniendo un ratio de
17.41% por lo que realemnte generar muy pocas venta a comparación de las
perdidas que hay en este territorio, por lo que se considera que no es
rentable y se debería de dejar de vender en este terriotorio.
