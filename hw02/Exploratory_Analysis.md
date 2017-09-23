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
knitr::kable(gapminder$country, caption="Frequency Table of Country")
```

    ## Warning in kable_markdown(x = structure(c("Afghanistan", "Afghanistan", :
    ## The table should have a header (column names)

|                          |
|:-------------------------|
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Afghanistan              |
| Albania                  |
| Albania                  |
| Albania                  |
| Albania                  |
| Albania                  |
| Albania                  |
| Albania                  |
| Albania                  |
| Albania                  |
| Albania                  |
| Albania                  |
| Albania                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Algeria                  |
| Angola                   |
| Angola                   |
| Angola                   |
| Angola                   |
| Angola                   |
| Angola                   |
| Angola                   |
| Angola                   |
| Angola                   |
| Angola                   |
| Angola                   |
| Angola                   |
| Argentina                |
| Argentina                |
| Argentina                |
| Argentina                |
| Argentina                |
| Argentina                |
| Argentina                |
| Argentina                |
| Argentina                |
| Argentina                |
| Argentina                |
| Argentina                |
| Australia                |
| Australia                |
| Australia                |
| Australia                |
| Australia                |
| Australia                |
| Australia                |
| Australia                |
| Australia                |
| Australia                |
| Australia                |
| Australia                |
| Austria                  |
| Austria                  |
| Austria                  |
| Austria                  |
| Austria                  |
| Austria                  |
| Austria                  |
| Austria                  |
| Austria                  |
| Austria                  |
| Austria                  |
| Austria                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bahrain                  |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Bangladesh               |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Belgium                  |
| Benin                    |
| Benin                    |
| Benin                    |
| Benin                    |
| Benin                    |
| Benin                    |
| Benin                    |
| Benin                    |
| Benin                    |
| Benin                    |
| Benin                    |
| Benin                    |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bolivia                  |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Bosnia and Herzegovina   |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Botswana                 |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Brazil                   |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Bulgaria                 |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burkina Faso             |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Burundi                  |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cambodia                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Cameroon                 |
| Canada                   |
| Canada                   |
| Canada                   |
| Canada                   |
| Canada                   |
| Canada                   |
| Canada                   |
| Canada                   |
| Canada                   |
| Canada                   |
| Canada                   |
| Canada                   |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Central African Republic |
| Chad                     |
| Chad                     |
| Chad                     |
| Chad                     |
| Chad                     |
| Chad                     |
| Chad                     |
| Chad                     |
| Chad                     |
| Chad                     |
| Chad                     |
| Chad                     |
| Chile                    |
| Chile                    |
| Chile                    |
| Chile                    |
| Chile                    |
| Chile                    |
| Chile                    |
| Chile                    |
| Chile                    |
| Chile                    |
| Chile                    |
| Chile                    |
| China                    |
| China                    |
| China                    |
| China                    |
| China                    |
| China                    |
| China                    |
| China                    |
| China                    |
| China                    |
| China                    |
| China                    |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Colombia                 |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Comoros                  |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Dem. Rep.         |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Congo, Rep.              |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Costa Rica               |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Cote d'Ivoire            |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Croatia                  |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Cuba                     |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Czech Republic           |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Denmark                  |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Djibouti                 |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Dominican Republic       |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Ecuador                  |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| Egypt                    |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| El Salvador              |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Equatorial Guinea        |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Eritrea                  |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Ethiopia                 |
| Finland                  |
| Finland                  |
| Finland                  |
| Finland                  |
| Finland                  |
| Finland                  |
| Finland                  |
| Finland                  |
| Finland                  |
| Finland                  |
| Finland                  |
| Finland                  |
| France                   |
| France                   |
| France                   |
| France                   |
| France                   |
| France                   |
| France                   |
| France                   |
| France                   |
| France                   |
| France                   |
| France                   |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gabon                    |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Gambia                   |
| Germany                  |
| Germany                  |
| Germany                  |
| Germany                  |
| Germany                  |
| Germany                  |
| Germany                  |
| Germany                  |
| Germany                  |
| Germany                  |
| Germany                  |
| Germany                  |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Ghana                    |
| Greece                   |
| Greece                   |
| Greece                   |
| Greece                   |
| Greece                   |
| Greece                   |
| Greece                   |
| Greece                   |
| Greece                   |
| Greece                   |
| Greece                   |
| Greece                   |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guatemala                |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea                   |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Guinea-Bissau            |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Haiti                    |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Honduras                 |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hong Kong, China         |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Hungary                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| Iceland                  |
| India                    |
| India                    |
| India                    |
| India                    |
| India                    |
| India                    |
| India                    |
| India                    |
| India                    |
| India                    |
| India                    |
| India                    |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Indonesia                |
| Iran                     |
| Iran                     |
| Iran                     |
| Iran                     |
| Iran                     |
| Iran                     |
| Iran                     |
| Iran                     |
| Iran                     |
| Iran                     |
| Iran                     |
| Iran                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Iraq                     |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Ireland                  |
| Israel                   |
| Israel                   |
| Israel                   |
| Israel                   |
| Israel                   |
| Israel                   |
| Israel                   |
| Israel                   |
| Israel                   |
| Israel                   |
| Israel                   |
| Israel                   |
| Italy                    |
| Italy                    |
| Italy                    |
| Italy                    |
| Italy                    |
| Italy                    |
| Italy                    |
| Italy                    |
| Italy                    |
| Italy                    |
| Italy                    |
| Italy                    |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Jamaica                  |
| Japan                    |
| Japan                    |
| Japan                    |
| Japan                    |
| Japan                    |
| Japan                    |
| Japan                    |
| Japan                    |
| Japan                    |
| Japan                    |
| Japan                    |
| Japan                    |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Jordan                   |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Kenya                    |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Dem. Rep.         |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Korea, Rep.              |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Kuwait                   |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lebanon                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Lesotho                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Liberia                  |
| Libya                    |
| Libya                    |
| Libya                    |
| Libya                    |
| Libya                    |
| Libya                    |
| Libya                    |
| Libya                    |
| Libya                    |
| Libya                    |
| Libya                    |
| Libya                    |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Madagascar               |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malawi                   |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Malaysia                 |
| Mali                     |
| Mali                     |
| Mali                     |
| Mali                     |
| Mali                     |
| Mali                     |
| Mali                     |
| Mali                     |
| Mali                     |
| Mali                     |
| Mali                     |
| Mali                     |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritania               |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mauritius                |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mexico                   |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Mongolia                 |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Montenegro               |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Morocco                  |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Mozambique               |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Myanmar                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Namibia                  |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Nepal                    |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| Netherlands              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| New Zealand              |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Nicaragua                |
| Niger                    |
| Niger                    |
| Niger                    |
| Niger                    |
| Niger                    |
| Niger                    |
| Niger                    |
| Niger                    |
| Niger                    |
| Niger                    |
| Niger                    |
| Niger                    |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Nigeria                  |
| Norway                   |
| Norway                   |
| Norway                   |
| Norway                   |
| Norway                   |
| Norway                   |
| Norway                   |
| Norway                   |
| Norway                   |
| Norway                   |
| Norway                   |
| Norway                   |
| Oman                     |
| Oman                     |
| Oman                     |
| Oman                     |
| Oman                     |
| Oman                     |
| Oman                     |
| Oman                     |
| Oman                     |
| Oman                     |
| Oman                     |
| Oman                     |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Pakistan                 |
| Panama                   |
| Panama                   |
| Panama                   |
| Panama                   |
| Panama                   |
| Panama                   |
| Panama                   |
| Panama                   |
| Panama                   |
| Panama                   |
| Panama                   |
| Panama                   |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Paraguay                 |
| Peru                     |
| Peru                     |
| Peru                     |
| Peru                     |
| Peru                     |
| Peru                     |
| Peru                     |
| Peru                     |
| Peru                     |
| Peru                     |
| Peru                     |
| Peru                     |
| Philippines              |
| Philippines              |
| Philippines              |
| Philippines              |
| Philippines              |
| Philippines              |
| Philippines              |
| Philippines              |
| Philippines              |
| Philippines              |
| Philippines              |
| Philippines              |
| Poland                   |
| Poland                   |
| Poland                   |
| Poland                   |
| Poland                   |
| Poland                   |
| Poland                   |
| Poland                   |
| Poland                   |
| Poland                   |
| Poland                   |
| Poland                   |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Portugal                 |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Puerto Rico              |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Reunion                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Romania                  |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Rwanda                   |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Sao Tome and Principe    |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Saudi Arabia             |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Senegal                  |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Serbia                   |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Sierra Leone             |
| Singapore                |
| Singapore                |
| Singapore                |
| Singapore                |
| Singapore                |
| Singapore                |
| Singapore                |
| Singapore                |
| Singapore                |
| Singapore                |
| Singapore                |
| Singapore                |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovak Republic          |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Slovenia                 |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| Somalia                  |
| South Africa             |
| South Africa             |
| South Africa             |
| South Africa             |
| South Africa             |
| South Africa             |
| South Africa             |
| South Africa             |
| South Africa             |
| South Africa             |
| South Africa             |
| South Africa             |
| Spain                    |
| Spain                    |
| Spain                    |
| Spain                    |
| Spain                    |
| Spain                    |
| Spain                    |
| Spain                    |
| Spain                    |
| Spain                    |
| Spain                    |
| Spain                    |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sri Lanka                |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Sudan                    |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Swaziland                |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Sweden                   |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Switzerland              |
| Syria                    |
| Syria                    |
| Syria                    |
| Syria                    |
| Syria                    |
| Syria                    |
| Syria                    |
| Syria                    |
| Syria                    |
| Syria                    |
| Syria                    |
| Syria                    |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Taiwan                   |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Tanzania                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Thailand                 |
| Togo                     |
| Togo                     |
| Togo                     |
| Togo                     |
| Togo                     |
| Togo                     |
| Togo                     |
| Togo                     |
| Togo                     |
| Togo                     |
| Togo                     |
| Togo                     |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Trinidad and Tobago      |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Tunisia                  |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Turkey                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| Uganda                   |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United Kingdom           |
| United States            |
| United States            |
| United States            |
| United States            |
| United States            |
| United States            |
| United States            |
| United States            |
| United States            |
| United States            |
| United States            |
| United States            |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Uruguay                  |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Venezuela                |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| Vietnam                  |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| West Bank and Gaza       |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Yemen, Rep.              |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zambia                   |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
| Zimbabwe                 |
