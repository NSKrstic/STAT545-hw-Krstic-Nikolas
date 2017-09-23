Exploratory Analysis
================
Nikolas Krstic
September 15, 2017

Load necessary packages/data
----------------------------

``` r
library(gapminder)
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(knitr)
```

Explore "gapminder"
-------------------

Gapminder is a list

``` r
typeof(gapminder)
```

    ## [1] "list"

Gapminder is a tibble (can also be considered a data.frame)

``` r
class(gapminder)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

Gapminder has 6 columns/variables

``` r
ncol(gapminder)
```

    ## [1] 6

Gapminder has 1704 rows/observations

``` r
nrow(gapminder)
```

    ## [1] 1704

A few other functions that can be applied to get the gapminder dimensions

``` r
dim(gapminder)
```

    ## [1] 1704    6

The data type of each variable: - country: Factor - continent: Factor - year: Integer - lifeExp: Numeric - pop: Integer - gdpPercap: Numeric

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

``` r
kable(summary(gapminder$country), caption="Frequency Table of Country")
```

|                          |     |
|:-------------------------|----:|
| Afghanistan              |   12|
| Albania                  |   12|
| Algeria                  |   12|
| Angola                   |   12|
| Argentina                |   12|
| Australia                |   12|
| Austria                  |   12|
| Bahrain                  |   12|
| Bangladesh               |   12|
| Belgium                  |   12|
| Benin                    |   12|
| Bolivia                  |   12|
| Bosnia and Herzegovina   |   12|
| Botswana                 |   12|
| Brazil                   |   12|
| Bulgaria                 |   12|
| Burkina Faso             |   12|
| Burundi                  |   12|
| Cambodia                 |   12|
| Cameroon                 |   12|
| Canada                   |   12|
| Central African Republic |   12|
| Chad                     |   12|
| Chile                    |   12|
| China                    |   12|
| Colombia                 |   12|
| Comoros                  |   12|
| Congo, Dem. Rep.         |   12|
| Congo, Rep.              |   12|
| Costa Rica               |   12|
| Cote d'Ivoire            |   12|
| Croatia                  |   12|
| Cuba                     |   12|
| Czech Republic           |   12|
| Denmark                  |   12|
| Djibouti                 |   12|
| Dominican Republic       |   12|
| Ecuador                  |   12|
| Egypt                    |   12|
| El Salvador              |   12|
| Equatorial Guinea        |   12|
| Eritrea                  |   12|
| Ethiopia                 |   12|
| Finland                  |   12|
| France                   |   12|
| Gabon                    |   12|
| Gambia                   |   12|
| Germany                  |   12|
| Ghana                    |   12|
| Greece                   |   12|
| Guatemala                |   12|
| Guinea                   |   12|
| Guinea-Bissau            |   12|
| Haiti                    |   12|
| Honduras                 |   12|
| Hong Kong, China         |   12|
| Hungary                  |   12|
| Iceland                  |   12|
| India                    |   12|
| Indonesia                |   12|
| Iran                     |   12|
| Iraq                     |   12|
| Ireland                  |   12|
| Israel                   |   12|
| Italy                    |   12|
| Jamaica                  |   12|
| Japan                    |   12|
| Jordan                   |   12|
| Kenya                    |   12|
| Korea, Dem. Rep.         |   12|
| Korea, Rep.              |   12|
| Kuwait                   |   12|
| Lebanon                  |   12|
| Lesotho                  |   12|
| Liberia                  |   12|
| Libya                    |   12|
| Madagascar               |   12|
| Malawi                   |   12|
| Malaysia                 |   12|
| Mali                     |   12|
| Mauritania               |   12|
| Mauritius                |   12|
| Mexico                   |   12|
| Mongolia                 |   12|
| Montenegro               |   12|
| Morocco                  |   12|
| Mozambique               |   12|
| Myanmar                  |   12|
| Namibia                  |   12|
| Nepal                    |   12|
| Netherlands              |   12|
| New Zealand              |   12|
| Nicaragua                |   12|
| Niger                    |   12|
| Nigeria                  |   12|
| Norway                   |   12|
| Oman                     |   12|
| Pakistan                 |   12|
| Panama                   |   12|
| (Other)                  |  516|
