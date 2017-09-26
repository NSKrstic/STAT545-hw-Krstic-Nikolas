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

**The data type of each variable:** \* country: Factor \* continent: Factor \* year: Integer \* lifeExp: Double \* pop: Integer \* gdpPercap: Double

Explore individual variables
----------------------------

**General Summary Statistics**

``` r
summary(gapminder)
```

    ##         country        continent        year         lifeExp     
    ##  Afghanistan:  12   Africa  :624   Min.   :1952   Min.   :23.60  
    ##  Albania    :  12   Americas:300   1st Qu.:1966   1st Qu.:48.20  
    ##  Algeria    :  12   Asia    :396   Median :1980   Median :60.71  
    ##  Angola     :  12   Europe  :360   Mean   :1980   Mean   :59.47  
    ##  Argentina  :  12   Oceania : 24   3rd Qu.:1993   3rd Qu.:70.85  
    ##  Australia  :  12                  Max.   :2007   Max.   :82.60  
    ##  (Other)    :1632                                                
    ##       pop              gdpPercap       
    ##  Min.   :6.001e+04   Min.   :   241.2  
    ##  1st Qu.:2.794e+06   1st Qu.:  1202.1  
    ##  Median :7.024e+06   Median :  3531.8  
    ##  Mean   :2.960e+07   Mean   :  7215.3  
    ##  3rd Qu.:1.959e+07   3rd Qu.:  9325.5  
    ##  Max.   :1.319e+09   Max.   :113523.1  
    ## 

**Frequency table of "continent" variable found within the dataset**

``` r
kable(t(as.matrix(table(gapminder$continent))))
```

|  Africa|  Americas|  Asia|  Europe|  Oceania|
|-------:|---------:|-----:|-------:|--------:|
|     624|       300|   396|     360|       24|

**Frequency table of "continent" variable found within the dataset**

``` r
kable(t(as.matrix(table(gapminder$country))))
```

|  Afghanistan|  Albania|  Algeria|  Angola|  Argentina|  Australia|  Austria|  Bahrain|  Bangladesh|  Belgium|  Benin|  Bolivia|  Bosnia and Herzegovina|  Botswana|  Brazil|  Bulgaria|  Burkina Faso|  Burundi|  Cambodia|  Cameroon|  Canada|  Central African Republic|  Chad|  Chile|  China|  Colombia|  Comoros|  Congo, Dem. Rep.|  Congo, Rep.|  Costa Rica|  Cote d'Ivoire|  Croatia|  Cuba|  Czech Republic|  Denmark|  Djibouti|  Dominican Republic|  Ecuador|  Egypt|  El Salvador|  Equatorial Guinea|  Eritrea|  Ethiopia|  Finland|  France|  Gabon|  Gambia|  Germany|  Ghana|  Greece|  Guatemala|  Guinea|  Guinea-Bissau|  Haiti|  Honduras|  Hong Kong, China|  Hungary|  Iceland|  India|  Indonesia|  Iran|  Iraq|  Ireland|  Israel|  Italy|  Jamaica|  Japan|  Jordan|  Kenya|  Korea, Dem. Rep.|  Korea, Rep.|  Kuwait|  Lebanon|  Lesotho|  Liberia|  Libya|  Madagascar|  Malawi|  Malaysia|  Mali|  Mauritania|  Mauritius|  Mexico|  Mongolia|  Montenegro|  Morocco|  Mozambique|  Myanmar|  Namibia|  Nepal|  Netherlands|  New Zealand|  Nicaragua|  Niger|  Nigeria|  Norway|  Oman|  Pakistan|  Panama|  Paraguay|  Peru|  Philippines|  Poland|  Portugal|  Puerto Rico|  Reunion|  Romania|  Rwanda|  Sao Tome and Principe|  Saudi Arabia|  Senegal|  Serbia|  Sierra Leone|  Singapore|  Slovak Republic|  Slovenia|  Somalia|  South Africa|  Spain|  Sri Lanka|  Sudan|  Swaziland|  Sweden|  Switzerland|  Syria|  Taiwan|  Tanzania|  Thailand|  Togo|  Trinidad and Tobago|  Tunisia|  Turkey|  Uganda|  United Kingdom|  United States|  Uruguay|  Venezuela|  Vietnam|  West Bank and Gaza|  Yemen, Rep.|  Zambia|  Zimbabwe|
|------------:|--------:|--------:|-------:|----------:|----------:|--------:|--------:|-----------:|--------:|------:|--------:|-----------------------:|---------:|-------:|---------:|-------------:|--------:|---------:|---------:|-------:|-------------------------:|-----:|------:|------:|---------:|--------:|-----------------:|------------:|-----------:|--------------:|--------:|-----:|---------------:|--------:|---------:|-------------------:|--------:|------:|------------:|------------------:|--------:|---------:|--------:|-------:|------:|-------:|--------:|------:|-------:|----------:|-------:|--------------:|------:|---------:|-----------------:|--------:|--------:|------:|----------:|-----:|-----:|--------:|-------:|------:|--------:|------:|-------:|------:|-----------------:|------------:|-------:|--------:|--------:|--------:|------:|-----------:|-------:|---------:|-----:|-----------:|----------:|-------:|---------:|-----------:|--------:|-----------:|--------:|--------:|------:|------------:|------------:|----------:|------:|--------:|-------:|-----:|---------:|-------:|---------:|-----:|------------:|-------:|---------:|------------:|--------:|--------:|-------:|----------------------:|-------------:|--------:|-------:|-------------:|----------:|----------------:|---------:|--------:|-------------:|------:|----------:|------:|----------:|-------:|------------:|------:|-------:|---------:|---------:|-----:|--------------------:|--------:|-------:|-------:|---------------:|--------------:|--------:|----------:|--------:|-------------------:|------------:|-------:|---------:|
|           12|       12|       12|      12|         12|         12|       12|       12|          12|       12|     12|       12|                      12|        12|      12|        12|            12|       12|        12|        12|      12|                        12|    12|     12|     12|        12|       12|                12|           12|          12|             12|       12|    12|              12|       12|        12|                  12|       12|     12|           12|                 12|       12|        12|       12|      12|     12|      12|       12|     12|      12|         12|      12|             12|     12|        12|                12|       12|       12|     12|         12|    12|    12|       12|      12|     12|       12|     12|      12|     12|                12|           12|      12|       12|       12|       12|     12|          12|      12|        12|    12|          12|         12|      12|        12|          12|       12|          12|       12|       12|     12|           12|           12|         12|     12|       12|      12|    12|        12|      12|        12|    12|           12|      12|        12|           12|       12|       12|      12|                     12|            12|       12|      12|            12|         12|               12|        12|       12|            12|     12|         12|     12|         12|      12|           12|     12|      12|        12|        12|    12|                   12|       12|      12|      12|              12|             12|       12|         12|       12|                  12|           12|      12|        12|
