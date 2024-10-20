Lab 7
================
Pedro Vargas
2024-10-20

``` r
library(readr)
library(tidyverse)
library(stringr)
library(tidytext)
library(lubridate)
library(stopwords)
library(wordcloud)
```

``` r
df <- read_csv("Health_and_Personal_Care.csv")

df$text <- str_replace_all(df$text, pattern = '\\"', replacement = '')

df %>% head()
```

    ## # A tibble: 6 × 8
    ##   rating title    text  product_id parent_id user_id timestamp verified_purchase
    ##    <dbl> <chr>    <chr> <chr>      <chr>     <chr>       <dbl> <lgl>            
    ## 1      4 12 mg i… This… B07TDSJZMR B07TDSJZ… AFKZEN…   1.58e12 TRUE             
    ## 2      5 Save th… Love… B08637FWWF B08637FW… AEVWAM…   1.60e12 TRUE             
    ## 3      5 Fantast… I ha… B07KJVGNN5 B07KJVGN… AHSPLD…   1.56e12 TRUE             
    ## 4      4 It hold… It's… B007HY7GC2 B092RP73… AEZGPL…   1.66e12 TRUE             
    ## 5      1 Not for… Didn… B08KYJLF5T B08KYJLF… AEQAYV…   1.64e12 TRUE             
    ## 6      5 Every h… I ha… B09GBMG83Z B09GBMG8… AFSKPY…   1.65e12 FALSE

``` r
meta <- read_csv("Health_and_Personal_Care_metadata.csv")
meta %>% head()
```

    ## # A tibble: 6 × 8
    ##   main_category title average_rating rating_number price store details parent_id
    ##   <chr>         <chr>          <dbl>         <dbl> <dbl> <chr> <chr>   <chr>    
    ## 1 Health & Per… Sili…            3.9             7  NA   Rzoe… 15 x 3… B07V346G…
    ## 2 Health & Per… iPho…            3.8             2  NA   ZHXIN ZHXIN,… B075W927…
    ## 3 Health & Per… Zig …            3.9             7  NA   <NA>  4.1 x … B01FB26V…
    ## 4 Health & Per… Stin…            4.1             6  21.4 Stin… Sting-… B01IAI29…
    ## 5 Health & Per… Heat…            3.3             8  NA   BiBO… 6.1 x … B08CMN38…
    ## 6 Health & Per… Ball…            4.6            19  NA   Tikt… Bachel… B07YJ5JB…

1.  Cuántos productos contienen reviews con las palabras “love”,
    “recommend” y “enjoy”?

``` r
words <- c(
  "love"
  ,"recommend"
  ,"enjoy"
)

positive_words <- paste(words, collapse = '|')

positive <- df %>%
  filter(str_detect(string = df$text,pattern = positive_words)) %>% 
  distinct(product_id)%>%
  count(name = "products_with_positive_review") 

positive
```

    ## # A tibble: 1 × 1
    ##   products_with_positive_review
    ##                           <int>
    ## 1                         23180

2.  De los reviews de la pregunta 1, encuentre el top 5 de las tiendas
    que los venden?

``` r
filtrados <- df %>%
  filter(str_detect(string = text, pattern = positive_words)) %>%
  distinct(product_id,parent_id)

tiendas <- filtrados %>%
  inner_join(meta, by = "parent_id") %>%
  filter(!is.na(store)) %>%
  group_by(store) %>%
  summarise(total_products = n_distinct(product_id)) %>%
  arrange(desc(total_products)) %>%
  slice_max(total_products, n = 5)%>%
  select(store)

tiendas
```

    ## # A tibble: 5 × 1
    ##   store      
    ##   <chr>      
    ## 1 HAARBB     
    ## 2 Eyekepper  
    ## 3 US Organic 
    ## 4 Andaz Press
    ## 5 Generic

3.  Genere un wordcloud sin stopwords de los reviews de la pregunta 1.

``` r
positive2 <- df %>%
  filter(str_detect(string = df$text,pattern = positive_words)) %>% 
  distinct(product_id,text)%>%
  select(text)

stop_words <- c(stopwords(language = "en"), stopwords(language = "es"))

words <- str_split(positive2$text[1:100], boundary("word")) %>% unlist()

no_stopwords <- words %>% 
  as_tibble() %>% 
  filter(!(value %in% stop_words)) %>% 
  group_by(value) %>% 
  summarise(freq = n())

wordcloud(no_stopwords$value, no_stopwords$freq)
```

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): definitely could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): available could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): everyone could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): monitor could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): comes could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): keychain could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): indicator could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): The could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): current could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): decrease could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): light could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Changing could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): viHealth could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): reasonable could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): make could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): AVG could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Accuracy could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): container could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): NOT could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): portability could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): read could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Although could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): effects could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): indicating could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): masks could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pressure could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): comfortable could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): around could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B09F3HTWYM
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): provided could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): review could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): shower could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): identify could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B09WDK7P8Y
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Unfortunately
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): guidance could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): sound could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): adjustment could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): br could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): quickly could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): lovely could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): within could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): money could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Time could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): European could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): I could not be fit
    ## on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): memory could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): never could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): deleting could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B08QH16BVY
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): brushes could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Type could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): enjoy could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): individually could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): monitors could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): wrapped could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): replace could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): irregular could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): competitive could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bags could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): clear could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): holding could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B09N92DMSH
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): video could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): piece could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): charging could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Cable could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): alarm could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): storage could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): sturdy could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): towel could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B08DTWKYC9
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): settings could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): quite could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): issues could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): falls could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): color could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): dry could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): videos could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hook could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): products could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Chinese could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): socks could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): inflation could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): expected could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Setting could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): reach could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Batteries could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): adjustable could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): two could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): results could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): atypical could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): respiration could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): colors could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pamphlet could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): case could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): commands could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): uses could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): VIDEOID could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): excellent could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): What could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hold could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B085VN5DQB
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): fresh could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): includes could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): others could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Vitamin could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): extremely could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Voice could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): apply could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B08KXWKDDM
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): every could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): another could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): talking could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): obese could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): flimsy could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): secure could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hope could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): inflammation could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): slots could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): drawbacks could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): black could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bought could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): buy could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): think could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): coded could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): need could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): accented could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Miecux could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): lightweight could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): This could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): accent could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B09FSNLQ21
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): turn could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hands could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): found could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): glasses could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B08HK71G16
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): difficult could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): There could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Position could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): keeps could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): blend could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): HealthSmart could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): multiple could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): mark could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): muscle could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): USB could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): outstanding could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): smaller could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): empty could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): decision could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): PANACARE could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): range could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Omron could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): batteries could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): complexion could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): difference could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): default could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): love could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): white could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): stores could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): leave could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): though could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): learn could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): people could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): They could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): nice could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): drying could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B081HCZPY2
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): priced could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): compression could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): cord could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): super could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): course could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): understand could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): heavily could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): exercise could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): design could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): C.P could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): small could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): packaging could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): husband could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): allow could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): also could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Up could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bright could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): One could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): getting could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): marked could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): handle could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): tried could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): without could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): dots could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): vials could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): highly could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): may could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): much could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): display could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): yet could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Each could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Series could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): mask could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): wonderful could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): beautiful could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): forget could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): next could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): received could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): store could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): easy could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Aleshon could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): matter could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): BP could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): loves could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): selecting could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): stomach could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): connect could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): program could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 17.99 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): associated could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): suitable could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Upper could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): almost could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pricey could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B09KXD6SNP
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): health could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): negative could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): amount could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): These could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 0885 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bath could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): support could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Timer could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): options could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): version could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): background could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): gold could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): included could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): yellow could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): clock could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): said could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Both could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN:B09CPYQ3PN
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): minute could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): upside could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Ziqing could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): feet could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Asobilor could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): front could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): sleeves could not
    ## be fit on page. It will not be plotted.

![](Lab7_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> 4. Generar un
wordcloud de los reviews de las 5 tiendas encontradas en la pregunta 2.
Deberá de incluir todos los reviews de esas 5 tiendas.

``` r
tiendas <- c("HAARBB","Eyekepper","US Organic","Andaz Press","Generic")

parents <- meta %>%
  filter(store %in% tiendas) %>%
  select(parent_id)

tabla <- df %>%
  inner_join(parents, by = "parent_id") %>%
  select(text)

words2 <- str_split(tabla$text[1:100], boundary("word")) %>% unlist()

no_stopwords2 <- words2 %>% 
  as_tibble() %>% 
  filter(!(value %in% stop_words)) %>% 
  group_by(value) %>% 
  summarise(freq = n())

wordcloud(no_stopwords2$value, no_stopwords2$freq)
```

![](Lab7_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> 5. Cuáles son
las 25 palabras más frecuentes de los reviews?

``` r
general <- str_split(df$text, boundary("word")) %>%
  unlist()

no_stopwords3 <- general %>%
  as_tibble() %>%
  filter(!(value %in% stop_words)) %>%
  group_by(value) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) 

general_25 <- no_stopwords3 %>%
  slice_max(freq, n = 25)

general_25
```

    ## # A tibble: 25 × 2
    ##    value     freq
    ##    <chr>    <int>
    ##  1 I       590664
    ##  2 br      136733
    ##  3 product 100609
    ##  4 The      94872
    ##  5 It       81421
    ##  6 use      80636
    ##  7 like     76352
    ##  8 great    71485
    ##  9 This     69662
    ## 10 one      64936
    ## # ℹ 15 more rows
