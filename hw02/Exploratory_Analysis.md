Exploratory Analysis
================
Nikolas Krstic
September 15, 2017

Load necessary packages/data
----------------------------

``` r
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(knitr))
```

Explore full "gapminder" dataset
--------------------------------

**Gapminder is a list**

``` r
typeof(gapminder)
```

    ## [1] "list"

**Gapminder is a tibble (can also be considered a data.frame)**

``` r
class(gapminder)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

**Gapminder has 6 columns/variables**

``` r
ncol(gapminder)
```

    ## [1] 6

**Gapminder has 1704 rows/observations**

``` r
nrow(gapminder)
```

    ## [1] 1704

**Another few functions that can be applied to get the gapminder dimensions. Particularly useful if handling data that's not necessarily two dimensional (arrays, etc.). Also, "str" provides information on the class of each variable.**

``` r
dim(gapminder)
```

    ## [1] 1704    6

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

**The data type of each variable:**

-   country: Factor
-   continent: Factor
-   year: Integer
-   lifeExp: Double
-   pop: Integer
-   gdpPercap: Double

Explore individual variables
----------------------------

**Frequency table of "continent" variable**

``` r
kable(t(as.matrix(table(gapminder$continent))))
```

|  Africa|  Americas|  Asia|  Europe|  Oceania|
|-------:|---------:|-----:|-------:|--------:|
|     624|       300|   396|     360|       24|

**Frequency table of "country" variable**

``` r
kable(t(as.matrix(table(gapminder$country))))
```

|  Afghanistan|  Albania|  Algeria|  Angola|  Argentina|  Australia|  Austria|  Bahrain|  Bangladesh|  Belgium|  Benin|  Bolivia|  Bosnia and Herzegovina|  Botswana|  Brazil|  Bulgaria|  Burkina Faso|  Burundi|  Cambodia|  Cameroon|  Canada|  Central African Republic|  Chad|  Chile|  China|  Colombia|  Comoros|  Congo, Dem. Rep.|  Congo, Rep.|  Costa Rica|  Cote d'Ivoire|  Croatia|  Cuba|  Czech Republic|  Denmark|  Djibouti|  Dominican Republic|  Ecuador|  Egypt|  El Salvador|  Equatorial Guinea|  Eritrea|  Ethiopia|  Finland|  France|  Gabon|  Gambia|  Germany|  Ghana|  Greece|  Guatemala|  Guinea|  Guinea-Bissau|  Haiti|  Honduras|  Hong Kong, China|  Hungary|  Iceland|  India|  Indonesia|  Iran|  Iraq|  Ireland|  Israel|  Italy|  Jamaica|  Japan|  Jordan|  Kenya|  Korea, Dem. Rep.|  Korea, Rep.|  Kuwait|  Lebanon|  Lesotho|  Liberia|  Libya|  Madagascar|  Malawi|  Malaysia|  Mali|  Mauritania|  Mauritius|  Mexico|  Mongolia|  Montenegro|  Morocco|  Mozambique|  Myanmar|  Namibia|  Nepal|  Netherlands|  New Zealand|  Nicaragua|  Niger|  Nigeria|  Norway|  Oman|  Pakistan|  Panama|  Paraguay|  Peru|  Philippines|  Poland|  Portugal|  Puerto Rico|  Reunion|  Romania|  Rwanda|  Sao Tome and Principe|  Saudi Arabia|  Senegal|  Serbia|  Sierra Leone|  Singapore|  Slovak Republic|  Slovenia|  Somalia|  South Africa|  Spain|  Sri Lanka|  Sudan|  Swaziland|  Sweden|  Switzerland|  Syria|  Taiwan|  Tanzania|  Thailand|  Togo|  Trinidad and Tobago|  Tunisia|  Turkey|  Uganda|  United Kingdom|  United States|  Uruguay|  Venezuela|  Vietnam|  West Bank and Gaza|  Yemen, Rep.|  Zambia|  Zimbabwe|
|------------:|--------:|--------:|-------:|----------:|----------:|--------:|--------:|-----------:|--------:|------:|--------:|-----------------------:|---------:|-------:|---------:|-------------:|--------:|---------:|---------:|-------:|-------------------------:|-----:|------:|------:|---------:|--------:|-----------------:|------------:|-----------:|--------------:|--------:|-----:|---------------:|--------:|---------:|-------------------:|--------:|------:|------------:|------------------:|--------:|---------:|--------:|-------:|------:|-------:|--------:|------:|-------:|----------:|-------:|--------------:|------:|---------:|-----------------:|--------:|--------:|------:|----------:|-----:|-----:|--------:|-------:|------:|--------:|------:|-------:|------:|-----------------:|------------:|-------:|--------:|--------:|--------:|------:|-----------:|-------:|---------:|-----:|-----------:|----------:|-------:|---------:|-----------:|--------:|-----------:|--------:|--------:|------:|------------:|------------:|----------:|------:|--------:|-------:|-----:|---------:|-------:|---------:|-----:|------------:|-------:|---------:|------------:|--------:|--------:|-------:|----------------------:|-------------:|--------:|-------:|-------------:|----------:|----------------:|---------:|--------:|-------------:|------:|----------:|------:|----------:|-------:|------------:|------:|-------:|---------:|---------:|-----:|--------------------:|--------:|-------:|-------:|---------------:|--------------:|--------:|----------:|--------:|-------------------:|------------:|-------:|---------:|
|           12|       12|       12|      12|         12|         12|       12|       12|          12|       12|     12|       12|                      12|        12|      12|        12|            12|       12|        12|        12|      12|                        12|    12|     12|     12|        12|       12|                12|           12|          12|             12|       12|    12|              12|       12|        12|                  12|       12|     12|           12|                 12|       12|        12|       12|      12|     12|      12|       12|     12|      12|         12|      12|             12|     12|        12|                12|       12|       12|     12|         12|    12|    12|       12|      12|     12|       12|     12|      12|     12|                12|           12|      12|       12|       12|       12|     12|          12|      12|        12|    12|          12|         12|      12|        12|          12|       12|          12|       12|       12|     12|           12|           12|         12|     12|       12|      12|    12|        12|      12|        12|    12|           12|      12|        12|           12|       12|       12|      12|                     12|            12|       12|      12|            12|         12|               12|        12|       12|            12|     12|         12|     12|         12|      12|           12|     12|      12|        12|        12|    12|                   12|       12|      12|      12|              12|             12|       12|         12|       12|                  12|           12|      12|        12|

**Frequency table of "year" variable**

``` r
kable(t(as.matrix(table(gapminder$year))))
```

|  1952|  1957|  1962|  1967|  1972|  1977|  1982|  1987|  1992|  1997|  2002|  2007|
|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
|   142|   142|   142|   142|   142|   142|   142|   142|   142|   142|   142|   142|

**Summary Statistics of "lifeExp" variable**

``` r
LE_SS = gapminder %>%
  summarize(Minimum = min(lifeExp), Maximum = max(lifeExp),
            Mean = mean(lifeExp), SD = sd(lifeExp))

kable(LE_SS)
```

|  Minimum|  Maximum|      Mean|        SD|
|--------:|--------:|---------:|---------:|
|   23.599|   82.603|  59.47444|  12.91711|

**Summary Statistics of "gdpPercap" variable**

``` r
GPC_SS = gapminder %>%
  summarize(Minimum = min(gdpPercap), Maximum = max(gdpPercap),
            Mean = mean(gdpPercap), SD = sd(gdpPercap))

kable(GPC_SS)
```

|   Minimum|   Maximum|      Mean|        SD|
|---------:|---------:|---------:|---------:|
|  241.1659|  113523.1|  7215.327|  9857.455|

**Summary Statistics of "pop" variable**

``` r
POP_SS = gapminder %>%
  summarize(Minimum = min(pop), Maximum = max(pop),
            Mean = mean(pop), SD = sd(pop))

kable(POP_SS)
```

|  Minimum|     Maximum|      Mean|         SD|
|--------:|-----------:|---------:|----------:|
|    60011|  1318683096|  29601212|  106157897|
