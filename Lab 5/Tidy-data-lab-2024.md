Laboratorio de Tidy Data
================
Tepi
9/28/2024

## Objetivos

1.  Familiarizarse con el concepto de *Tidy Data* basado en el trabajo
    de Hadley Wickham.
2.  Aprender a utilizar la funcion `melt`. y la función `gather()` y las
    funciones `pivot` de `dplyr`.

``` r
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
```

\###pivot_longer

## Ejemplos:

Generaremos el dataset teórico que vimos en la presentación de la clase.

``` r
df <- data.frame(row = LETTERS[1:3], a = 1:3, b = 4:6, c = 7:9)
df
```

    ##   row a b c
    ## 1   A 1 4 7
    ## 2   B 2 5 8
    ## 3   C 3 6 9

`pivot_longer`de la libreria *dplyr*:

Pivot longer tiene como parametros las columnas que se van a
transformar, el nombre que va a tener esas columnas, y los valores.

Primer ejemplo: Usando pivot longer definiendo las columnas que no
queremos transformar.

``` r
df %>% 
  pivot_longer(cols = !row, names_to = "letters", values_to = "vals")
```

    ## # A tibble: 9 × 3
    ##   row   letters  vals
    ##   <chr> <chr>   <int>
    ## 1 A     a           1
    ## 2 A     b           4
    ## 3 A     c           7
    ## 4 B     a           2
    ## 5 B     b           5
    ## 6 B     c           8
    ## 7 C     a           3
    ## 8 C     b           6
    ## 9 C     c           9

Segundo ejemplo: Usando pivot longer definiendo las columnas que
queremos transformar.

``` r
df %>% 
  pivot_longer( cols = c(a,b,c), names_to = "letters", values_to = "vals")
```

    ## # A tibble: 9 × 3
    ##   row   letters  vals
    ##   <chr> <chr>   <int>
    ## 1 A     a           1
    ## 2 A     b           4
    ## 3 A     c           7
    ## 4 B     a           2
    ## 5 B     b           5
    ## 6 B     c           8
    ## 7 C     a           3
    ## 8 C     b           6
    ## 9 C     c           9

# Ejercicio

Exportemos el archivo llamado *raw.csv*

Este dataset es de la World Health Organization y cuenta los casos de
Tuberculosis en varios paises. m es masculino, f es femenino y los
numeros son el rango de edad en el que se encuentra la persona.

``` r
raw<-read.csv("raw.csv")
head(raw)
```

    ##     X country year m014 m1524 m2534 m3544 m4554 m5564 m65 mu f014 f1524 f2534
    ## 1  11      AD 2000    0     0     1     0     0     0   0 NA   NA    NA    NA
    ## 2  37      AE 2000    2     4     4     6     5    12  10 NA    3    16     1
    ## 3  61      AF 2000   52   228   183   149   129    94  80 NA   93   414   565
    ## 4  88      AG 2000    0     0     0     0     0     0   1 NA    1     1     1
    ## 5 137      AL 2000    2    19    21    14    24    19  16 NA    3    11    10
    ## 6 166      AM 2000    2   152   130   131    63    26  21 NA    1    24    27
    ##   f3544 f4554 f5564 f65 fu
    ## 1    NA    NA    NA  NA NA
    ## 2     3     0     0   4 NA
    ## 3   339   205    99  36 NA
    ## 4     0     0     0   0 NA
    ## 5     8     8     5  11 NA
    ## 6    24     8     8   4 NA

# Laboratorio

Transformar el dataset a formato Tidy.

Primer paso: Bajar las columnas que tienen variables a filas.

``` r
summary(df)
```

    ##      row                  a             b             c      
    ##  Length:3           Min.   :1.0   Min.   :4.0   Min.   :7.0  
    ##  Class :character   1st Qu.:1.5   1st Qu.:4.5   1st Qu.:7.5  
    ##  Mode  :character   Median :2.0   Median :5.0   Median :8.0  
    ##                     Mean   :2.0   Mean   :5.0   Mean   :8.0  
    ##                     3rd Qu.:2.5   3rd Qu.:5.5   3rd Qu.:8.5  
    ##                     Max.   :3.0   Max.   :6.0   Max.   :9.0

``` r
raw_longer <-raw %>%
  pivot_longer(cols = !c(X,country,year), names_to = "GR", values_to = "Cases")

raw_longer %>%
  head()
```

    ## # A tibble: 6 × 5
    ##       X country  year GR    Cases
    ##   <int> <chr>   <int> <chr> <int>
    ## 1    11 AD       2000 m014      0
    ## 2    11 AD       2000 m1524     0
    ## 3    11 AD       2000 m2534     1
    ## 4    11 AD       2000 m3544     0
    ## 5    11 AD       2000 m4554     0
    ## 6    11 AD       2000 m5564     0

Segundo paso: Separar las variables que estan en la misma columna
extrayendo la variable de sexo (m,f).

Sugerencia, utilizar la funcion `str_sub()` del paquete **stringr**

``` r
raw_longer$sex <- str_sub(raw_longer$GR, start = 1,end = 1)
raw_longer$age <- str_sub(raw_longer$GR, start = 2, end = nchar(raw_longer$GR))

raw_longer <- raw_longer %>%
  select(-GR)

head(raw_longer)
```

    ## # A tibble: 6 × 6
    ##       X country  year Cases sex   age  
    ##   <int> <chr>   <int> <int> <chr> <chr>
    ## 1    11 AD       2000     0 m     014  
    ## 2    11 AD       2000     0 m     1524 
    ## 3    11 AD       2000     1 m     2534 
    ## 4    11 AD       2000     0 m     3544 
    ## 5    11 AD       2000     0 m     4554 
    ## 6    11 AD       2000     0 m     5564

Tercer paso: Extraer y formatear la variable de edad.

``` r
ages <- c("0-4", "5-14", "0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+", NA)

raw_longer$Age <- factor(recode(raw_longer$age,
                             "04" = "0-4", 
                             "514" = "5-14", 
                             "014" = "0-14", 
                             "1524" = "15-24", 
                             "2534" = "25-34", 
                             "3544" = "35-44", 
                             "4554" = "45-54", 
                             "5564" = "55-64", 
                             "65" = "65+", 
                             "u" = NA_character_),
                      levels = ages)

raw_longer <- raw_longer %>%
  select(-age)

head(raw_longer)
```

    ## # A tibble: 6 × 6
    ##       X country  year Cases sex   Age  
    ##   <int> <chr>   <int> <int> <chr> <fct>
    ## 1    11 AD       2000     0 m     0-14 
    ## 2    11 AD       2000     0 m     15-24
    ## 3    11 AD       2000     1 m     25-34
    ## 4    11 AD       2000     0 m     35-44
    ## 5    11 AD       2000     0 m     45-54
    ## 6    11 AD       2000     0 m     55-64
