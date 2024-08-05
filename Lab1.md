Lab1
================
Pedro Vargas
2024-08-04

PROBLEMA 1

``` r
library(readxl)
library(openxlsx)
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
tabla1 <- read.xlsx("01-2023.xlsx")
tabla2 <- read.xlsx("02-2023.xlsx")
tabla3 <- read.xlsx("03-2023.xlsx")
tabla4 <- read.xlsx("04-2023.xlsx")
tabla5 <- read.xlsx("05-2023.xlsx")
tabla6 <- read.xlsx("06-2023.xlsx")
tabla7 <- read.xlsx("07-2023.xlsx")
tabla8 <- read.xlsx("08-2023.xlsx")
tabla9 <- read.xlsx("09-2023.xlsx")
tabla10 <- read.xlsx("10-2023.xlsx")
tabla11 <- read.xlsx("11-2023.xlsx")

tabla7 <- tabla7 %>%
  select(-TIPO)
tabla8 <- tabla8 %>%
  select(-TIPO,-X10)
tabla9 <- tabla9 %>%
  select(-TIPO)
tabla10 <- tabla10 %>%
  select(-TIPO)
tabla11 <- tabla11 %>%
  select(-TIPO)

tabla1$FECHA <- '01-2023'
tabla2$FECHA <- '02-2023'
tabla3$FECHA <- '03-2023'
tabla4$FECHA <- '04-2023'
tabla5$FECHA <- '05-2023'
tabla6$FECHA <- '06-2023'
tabla7$FECHA <- '07-2023'
tabla8$FECHA <- '08-2023'
tabla9$FECHA <- '09-2023'
tabla10$FECHA <- '10-2023'
tabla11$FECHA <- '11-2023'

tabla_final <- bind_rows(tabla1,tabla2,tabla3,tabla4,tabla5,tabla6,tabla7,tabla8,tabla9,tabla10,tabla11)

write.xlsx(tabla_final,"Entregas2023.xlsx")

head(tabla_final)
```

    ##   COD_VIAJE                                       CLIENTE UBICACION CANTIDAD
    ## 1  10000001       EL PINCHE OBELISCO / Despacho a cliente     76002     1200
    ## 2  10000002               TAQUERIA EL CHINITO |||Faltante     76002     1433
    ## 3  10000003      TIENDA LA BENDICION / Despacho a cliente     76002     1857
    ## 4  10000004                           TAQUERIA EL CHINITO     76002      339
    ## 5  10000005 CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001     1644
    ## 6  10000006                       UBIQUO LABS |||FALTANTE     76001     1827
    ##                          PILOTO      Q CREDITO        UNIDAD   FECHA
    ## 1       Fernando Mariano Berrio 300.00      30 Camion Grande 01-2023
    ## 2        Hector Aragones Frutos 358.25      90 Camion Grande 01-2023
    ## 3          Pedro Alvarez Parejo 464.25      60 Camion Grande 01-2023
    ## 4          Angel Valdez Alegria  84.75      30         Panel 01-2023
    ## 5 Juan Francisco Portillo Gomez 411.00      30 Camion Grande 01-2023
    ## 6             Luis Jaime Urbano 456.75      30 Camion Grande 01-2023

``` r
nrow(tabla_final)
```

    ## [1] 2180

PROBLEMA 2

``` r
lista_vectores <- list(
  sample(1:5,size = 10,replace = TRUE),
  sample(20:45,size = 50,replace = TRUE),
  sample(50:100,size = 100,replace = TRUE)
)

calculo_moda <- function(vec){
  return(as.numeric(names(which.max(table(vec)))))
}

modas <- lapply(lista_vectores,calculo_moda)
print(lista_vectores)
```

    ## [[1]]
    ##  [1] 1 4 3 5 2 1 3 3 4 2
    ## 
    ## [[2]]
    ##  [1] 26 44 36 21 39 35 41 31 43 28 21 33 29 42 24 34 38 45 36 20 38 26 21 38 33
    ## [26] 27 27 26 22 29 23 33 37 35 43 38 36 36 41 36 20 43 41 28 41 23 24 43 36 20
    ## 
    ## [[3]]
    ##   [1]  71  59  70  60  78  63  66  90  61  58  85  86  93  78  89  61  53  72
    ##  [19]  71  90  80 100  92  52  76  92  67  57  53  72 100  58  66  66  61  60
    ##  [37]  85  80  96  73  67  73  53  88  96  85  87  72  87  64  60  84  53  50
    ##  [55]  54  89  79  51  96  75  94  92  71 100 100  83  81  93 100  59  94  64
    ##  [73]  84  94  76  92  83  88  91  71  63  64  85  97  59  60  67  92  99  52
    ##  [91]  74  95  74  61  80  92  78  90  56  93

``` r
print(modas)
```

    ## [[1]]
    ## [1] 3
    ## 
    ## [[2]]
    ## [1] 36
    ## 
    ## [[3]]
    ## [1] 92

PROBLEMA 3

``` r
library(readr)
ParqueVehicular <- read_delim('ParqueVehicularEnero2019.txt',delim = '|')
```

    ## New names:
    ## • `` -> `...11`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 2435294 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "|"
    ## chr (8): MES, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO, MODELO_VEHICULO, LINEA_...
    ## dbl (2): ANIO_ALZA, CANTIDAD
    ## lgl (1): ...11
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ParqueVehicular <- ParqueVehicular %>%
  select(-...11)

nrow(ParqueVehicular)
```

    ## [1] 2435294

``` r
head(ParqueVehicular)
```

    ## # A tibble: 6 × 10
    ##   ANIO_ALZA MES   NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##       <dbl> <chr> <chr>               <chr>            <chr>          
    ## 1      2007 05    HUEHUETENANGO       "HUEHUETENANGO"  2007           
    ## 2      2007 05    EL PROGRESO         "EL JICARO"      2007           
    ## 3      2007 05    SAN MARCOS          "OCOS"           2007           
    ## 4      2007 05    ESCUINTLA           "SAN JOS\xc9"    2006           
    ## 5      2007 05    JUTIAPA             "MOYUTA"         2007           
    ## 6      2007 05    GUATEMALA           "FRAIJANES"      1997           
    ## # ℹ 5 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>
