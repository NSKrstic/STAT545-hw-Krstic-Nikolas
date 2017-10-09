Gapminder\_Join\_and\_Reshape
================
Nikolas Krstic
October 7, 2017

``` r
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(reshape))
```

    ## Warning: package 'reshape' was built under R version 3.4.2

``` r
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(kableExtra))
```

    ## Warning: package 'kableExtra' was built under R version 3.4.2

Data Reshaping
--------------

#### Activity 2: Annual Life Expectancy for Multiple Countries

``` r
chosen_countries = c("United Kingdom", "Spain", "France",
                        "Germany", "Serbia", "Italy", "Sweden")

lifeExp_countries = gapminder %>%
  filter(country %in% chosen_countries) %>%
  select(country, lifeExp, year) %>%
  group_by(country, year) %>%
  summarize(mean_lifeExp = mean(lifeExp)) %>%
  dcast(year ~ country)
```

    ## Using mean_lifeExp as value column: use value.var to override.

``` r
kable(lifeExp_countries, col.names = c("Year", names(lifeExp_countries)[2:ncol(lifeExp_countries)]))
```

|  Year|  France|  Germany|   Italy|  Serbia|   Spain|  Sweden|  United Kingdom|
|-----:|-------:|--------:|-------:|-------:|-------:|-------:|---------------:|
|  1952|  67.410|   67.500|  65.940|  57.996|  64.940|  71.860|          69.180|
|  1957|  68.930|   69.100|  67.810|  61.685|  66.660|  72.490|          70.420|
|  1962|  70.510|   70.300|  69.240|  64.531|  69.690|  73.370|          70.760|
|  1967|  71.550|   70.800|  71.060|  66.914|  71.440|  74.160|          71.360|
|  1972|  72.380|   71.000|  72.190|  68.700|  73.060|  74.720|          72.010|
|  1977|  73.830|   72.500|  73.480|  70.300|  74.390|  75.440|          72.760|
|  1982|  74.890|   73.800|  74.980|  70.162|  76.300|  76.420|          74.040|
|  1987|  76.340|   74.847|  76.420|  71.218|  76.900|  77.190|          75.007|
|  1992|  77.460|   76.070|  77.440|  71.659|  77.570|  78.160|          76.420|
|  1997|  78.640|   77.340|  78.820|  72.232|  78.770|  79.390|          77.218|
|  2002|  79.590|   78.670|  80.240|  73.213|  79.780|  80.040|          78.471|
|  2007|  80.657|   79.406|  80.546|  74.002|  80.941|  80.884|          79.425|

``` r
lifeExp_countries %>%
  ggplot(aes(x=Serbia, y=Sweden)) +
  theme_bw() +
  geom_point(colour="red") +
  geom_line() +
  labs(x="Life Expectancy in Serbia", y="Life Expectancy in Sweden", title="Life Expectancy during 1952-2007")
```

![](Gapminder_Join_and_Reshape_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

#### Activity 3: Annual Maximum Life Expectancy within each Continent

``` r
lifeExp_continents = gapminder %>%
  select(continent, lifeExp, year) %>%
  group_by(continent, year) %>%
  summarize(max_lifeExp = max(lifeExp)) %>%
  dcast(year ~ continent)
```

    ## Using max_lifeExp as value column: use value.var to override.

``` r
kable(lifeExp_continents, col.names = c("Year", names(lifeExp_continents)[2:ncol(lifeExp_continents)]))
```

|  Year|  Africa|  Americas|    Asia|  Europe|  Oceania|
|-----:|-------:|---------:|-------:|-------:|--------:|
|  1952|  52.724|    68.750|  65.390|  72.670|   69.390|
|  1957|  58.089|    69.960|  67.840|  73.470|   70.330|
|  1962|  60.246|    71.300|  69.390|  73.680|   71.240|
|  1967|  61.557|    72.130|  71.430|  74.160|   71.520|
|  1972|  64.274|    72.880|  73.420|  74.720|   71.930|
|  1977|  67.064|    74.210|  75.380|  76.110|   73.490|
|  1982|  69.885|    75.760|  77.110|  76.990|   74.740|
|  1987|  71.913|    76.860|  78.670|  77.410|   76.320|
|  1992|  73.615|    77.950|  79.360|  78.770|   77.560|
|  1997|  74.772|    78.610|  80.690|  79.390|   78.830|
|  2002|  75.744|    79.770|  82.000|  80.620|   80.370|
|  2007|  76.442|    80.653|  82.603|  81.757|   81.235|

``` r
lifeExp_continents %>%
  select(-year) %>%
  plot(las=2, pch=16)
```

![](Gapminder_Join_and_Reshape_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

So, what's advantageous about this wide data format is that it's somewhat easier to generate pairwise combinations of each of the continents, to examine the trends in each of the scatterplots.

I was attempting to figure out whether there's a good way to implement this via ggplot, but it seems that ggplot typically accepts data in the long data format. It's possible to do so with one pair (as done in the previous section), but performing multiple combinations is somewhat messy.

#### Activity 4: Minimum and Maximum Life Expectancies in each Continent

``` r
LE_Table = gapminder %>%
  select(year, continent, country, lifeExp) %>%
  group_by(year, continent) %>%
  filter(min_rank(desc(lifeExp)) < 2 | min_rank(lifeExp) < 2) %>% 
  arrange(year, continent) %>%
  #Generate boolean to identify min and max LEs
  mutate(max_bool = ifelse(max(lifeExp) == lifeExp, "Max_LE", "Min_LE")) %>%
  #dcast on this boolean, causes some NAs to pop up initially
  dcast(year+continent+country ~ max_bool, value.var="lifeExp") %>%
  #Repeate boolean generation on which country had the max or min LE
  mutate(max_bool = ifelse(!is.na(Max_LE), "Max_Country", "Min_Country")) %>%
  #dcast on this boolean too, results in disjoint NAs between the four columns made
  dcast(year+continent+Max_LE+Min_LE ~ max_bool, value.var="country") %>%
  group_by(year, continent) %>%
  #summarize across the rows to remove the NAs and combine the rows together
  summarize_all(funs(na.omit(.))) %>%
  arrange(continent, continent)

kable(LE_Table, col.names=c("Year", "Continent", "Max_LE", "Min_LE", "Country_with_Max_LE", "Country_with_Min_LE"))
```

|  Year| Continent |  Max\_LE|  Min\_LE| Country\_with\_Max\_LE | Country\_with\_Min\_LE |
|-----:|:----------|--------:|--------:|:-----------------------|:-----------------------|
|  1952| Africa    |   52.724|   30.000| Reunion                | Gambia                 |
|  1957| Africa    |   58.089|   31.570| Mauritius              | Sierra Leone           |
|  1962| Africa    |   60.246|   32.767| Mauritius              | Sierra Leone           |
|  1967| Africa    |   61.557|   34.113| Mauritius              | Sierra Leone           |
|  1972| Africa    |   64.274|   35.400| Reunion                | Sierra Leone           |
|  1977| Africa    |   67.064|   36.788| Reunion                | Sierra Leone           |
|  1982| Africa    |   69.885|   38.445| Reunion                | Sierra Leone           |
|  1987| Africa    |   71.913|   39.906| Reunion                | Angola                 |
|  1992| Africa    |   73.615|   23.599| Reunion                | Rwanda                 |
|  1997| Africa    |   74.772|   36.087| Reunion                | Rwanda                 |
|  2002| Africa    |   75.744|   39.193| Reunion                | Zambia                 |
|  2007| Africa    |   76.442|   39.613| Reunion                | Swaziland              |
|  1952| Americas  |   68.750|   37.579| Canada                 | Haiti                  |
|  1957| Americas  |   69.960|   40.696| Canada                 | Haiti                  |
|  1962| Americas  |   71.300|   43.428| Canada                 | Bolivia                |
|  1967| Americas  |   72.130|   45.032| Canada                 | Bolivia                |
|  1972| Americas  |   72.880|   46.714| Canada                 | Bolivia                |
|  1977| Americas  |   74.210|   49.923| Canada                 | Haiti                  |
|  1982| Americas  |   75.760|   51.461| Canada                 | Haiti                  |
|  1987| Americas  |   76.860|   53.636| Canada                 | Haiti                  |
|  1992| Americas  |   77.950|   55.089| Canada                 | Haiti                  |
|  1997| Americas  |   78.610|   56.671| Canada                 | Haiti                  |
|  2002| Americas  |   79.770|   58.137| Canada                 | Haiti                  |
|  2007| Americas  |   80.653|   60.916| Canada                 | Haiti                  |
|  1952| Asia      |   65.390|   28.801| Israel                 | Afghanistan            |
|  1957| Asia      |   67.840|   30.332| Israel                 | Afghanistan            |
|  1962| Asia      |   69.390|   31.997| Israel                 | Afghanistan            |
|  1967| Asia      |   71.430|   34.020| Japan                  | Afghanistan            |
|  1972| Asia      |   73.420|   36.088| Japan                  | Afghanistan            |
|  1977| Asia      |   75.380|   31.220| Japan                  | Cambodia               |
|  1982| Asia      |   77.110|   39.854| Japan                  | Afghanistan            |
|  1987| Asia      |   78.670|   40.822| Japan                  | Afghanistan            |
|  1992| Asia      |   79.360|   41.674| Japan                  | Afghanistan            |
|  1997| Asia      |   80.690|   41.763| Japan                  | Afghanistan            |
|  2002| Asia      |   82.000|   42.129| Japan                  | Afghanistan            |
|  2007| Asia      |   82.603|   43.828| Japan                  | Afghanistan            |
|  1952| Europe    |   72.670|   43.585| Norway                 | Turkey                 |
|  1957| Europe    |   73.470|   48.079| Iceland                | Turkey                 |
|  1962| Europe    |   73.680|   52.098| Iceland                | Turkey                 |
|  1967| Europe    |   74.160|   54.336| Sweden                 | Turkey                 |
|  1972| Europe    |   74.720|   57.005| Sweden                 | Turkey                 |
|  1977| Europe    |   76.110|   59.507| Iceland                | Turkey                 |
|  1982| Europe    |   76.990|   61.036| Iceland                | Turkey                 |
|  1987| Europe    |   77.410|   63.108| Switzerland            | Turkey                 |
|  1992| Europe    |   78.770|   66.146| Iceland                | Turkey                 |
|  1997| Europe    |   79.390|   68.835| Sweden                 | Turkey                 |
|  2002| Europe    |   80.620|   70.845| Switzerland            | Turkey                 |
|  2007| Europe    |   81.757|   71.777| Iceland                | Turkey                 |
|  1952| Oceania   |   69.390|   69.120| New Zealand            | Australia              |
|  1957| Oceania   |   70.330|   70.260| Australia              | New Zealand            |
|  1962| Oceania   |   71.240|   70.930| New Zealand            | Australia              |
|  1967| Oceania   |   71.520|   71.100| New Zealand            | Australia              |
|  1972| Oceania   |   71.930|   71.890| Australia              | New Zealand            |
|  1977| Oceania   |   73.490|   72.220| Australia              | New Zealand            |
|  1982| Oceania   |   74.740|   73.840| Australia              | New Zealand            |
|  1987| Oceania   |   76.320|   74.320| Australia              | New Zealand            |
|  1992| Oceania   |   77.560|   76.330| Australia              | New Zealand            |
|  1997| Oceania   |   78.830|   77.550| Australia              | New Zealand            |
|  2002| Oceania   |   80.370|   79.110| Australia              | New Zealand            |
|  2007| Oceania   |   81.235|   80.204| Australia              | New Zealand            |

This problem was a little more complicated to approach. Initially, I had only applied dcast to the table, but this resulted in many NA values (Countries were columns and the values were the life expectancies) and generally looked messy. The end result that I wanted was 4 columns, each indicating the min or max life expectancy or the country that had min or max life expectancy. There might be a more efficient way of doing that, but what I have done is pretty good and clear.

Data Joining
------------

#### Activity 1: Gapminder Data Joining

``` r
chosen_countries = c("United Kingdom", "Spain", "France",
                        "Germany", "Serbia", "Italy", "Sweden")

New_DF = data.frame(country = c("United Kingdom", "Spain", "France", "Germany", "Serbia", "Italy", "Sweden"), Language = c("English", "Spanish", "French", "German", "Serbian", "Italian", "Swedish"), Capital = c("London", "Madrid", "Paris", "Berlin", "Belgrade", "Rome", "Stockholm"), NATO_Member = c("Yes", "Yes", "Yes", "Yes", "No", "Yes", "No"))

Old_DF = gapminder %>%
  filter(country %in% chosen_countries)
```

##### left\_join

``` r
kable(left_join(Old_DF, New_DF), format="html") %>%
  kable_styling() %>%
  scroll_box(width = "1000px", height = "500px")
```

    ## Joining, by = "country"

    ## Warning: Column `country` joining factors with different levels, coercing
    ## to character vector

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:left;">
continent
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
lifeExp
</th>
<th style="text-align:right;">
pop
</th>
<th style="text-align:right;">
gdpPercap
</th>
<th style="text-align:left;">
Language
</th>
<th style="text-align:left;">
Capital
</th>
<th style="text-align:left;">
NATO\_Member
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.410
</td>
<td style="text-align:right;">
42459667
</td>
<td style="text-align:right;">
7029.809
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
68.930
</td>
<td style="text-align:right;">
44310863
</td>
<td style="text-align:right;">
8662.835
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.510
</td>
<td style="text-align:right;">
47124000
</td>
<td style="text-align:right;">
10560.486
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.550
</td>
<td style="text-align:right;">
49569000
</td>
<td style="text-align:right;">
12999.918
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.380
</td>
<td style="text-align:right;">
51732000
</td>
<td style="text-align:right;">
16107.192
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.830
</td>
<td style="text-align:right;">
53165019
</td>
<td style="text-align:right;">
18292.635
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.890
</td>
<td style="text-align:right;">
54433565
</td>
<td style="text-align:right;">
20293.897
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.340
</td>
<td style="text-align:right;">
55630100
</td>
<td style="text-align:right;">
22066.442
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.460
</td>
<td style="text-align:right;">
57374179
</td>
<td style="text-align:right;">
24703.796
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.640
</td>
<td style="text-align:right;">
58623428
</td>
<td style="text-align:right;">
25889.785
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.590
</td>
<td style="text-align:right;">
59925035
</td>
<td style="text-align:right;">
28926.032
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.657
</td>
<td style="text-align:right;">
61083916
</td>
<td style="text-align:right;">
30470.017
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.500
</td>
<td style="text-align:right;">
69145952
</td>
<td style="text-align:right;">
7144.114
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
69.100
</td>
<td style="text-align:right;">
71019069
</td>
<td style="text-align:right;">
10187.827
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
73739117
</td>
<td style="text-align:right;">
12902.463
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
70.800
</td>
<td style="text-align:right;">
76368453
</td>
<td style="text-align:right;">
14745.626
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
71.000
</td>
<td style="text-align:right;">
78717088
</td>
<td style="text-align:right;">
18016.180
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.500
</td>
<td style="text-align:right;">
78160773
</td>
<td style="text-align:right;">
20512.921
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
73.800
</td>
<td style="text-align:right;">
78335266
</td>
<td style="text-align:right;">
22031.533
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
74.847
</td>
<td style="text-align:right;">
77718298
</td>
<td style="text-align:right;">
24639.186
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.070
</td>
<td style="text-align:right;">
80597764
</td>
<td style="text-align:right;">
26505.303
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.340
</td>
<td style="text-align:right;">
82011073
</td>
<td style="text-align:right;">
27788.884
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.670
</td>
<td style="text-align:right;">
82350671
</td>
<td style="text-align:right;">
30035.802
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.406
</td>
<td style="text-align:right;">
82400996
</td>
<td style="text-align:right;">
32170.374
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
65.940
</td>
<td style="text-align:right;">
47666000
</td>
<td style="text-align:right;">
4931.404
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
67.810
</td>
<td style="text-align:right;">
49182000
</td>
<td style="text-align:right;">
6248.656
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.240
</td>
<td style="text-align:right;">
50843200
</td>
<td style="text-align:right;">
8243.582
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.060
</td>
<td style="text-align:right;">
52667100
</td>
<td style="text-align:right;">
10022.401
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.190
</td>
<td style="text-align:right;">
54365564
</td>
<td style="text-align:right;">
12269.274
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.480
</td>
<td style="text-align:right;">
56059245
</td>
<td style="text-align:right;">
14255.985
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.980
</td>
<td style="text-align:right;">
56535636
</td>
<td style="text-align:right;">
16537.483
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
56729703
</td>
<td style="text-align:right;">
19207.235
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.440
</td>
<td style="text-align:right;">
56840847
</td>
<td style="text-align:right;">
22013.645
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.820
</td>
<td style="text-align:right;">
57479469
</td>
<td style="text-align:right;">
24675.024
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.240
</td>
<td style="text-align:right;">
57926999
</td>
<td style="text-align:right;">
27968.098
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.546
</td>
<td style="text-align:right;">
58147733
</td>
<td style="text-align:right;">
28569.720
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
57.996
</td>
<td style="text-align:right;">
6860147
</td>
<td style="text-align:right;">
3581.459
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
61.685
</td>
<td style="text-align:right;">
7271135
</td>
<td style="text-align:right;">
4981.091
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
64.531
</td>
<td style="text-align:right;">
7616060
</td>
<td style="text-align:right;">
6289.629
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
66.914
</td>
<td style="text-align:right;">
7971222
</td>
<td style="text-align:right;">
7991.707
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
68.700
</td>
<td style="text-align:right;">
8313288
</td>
<td style="text-align:right;">
10522.067
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
8686367
</td>
<td style="text-align:right;">
12980.670
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
70.162
</td>
<td style="text-align:right;">
9032824
</td>
<td style="text-align:right;">
15181.093
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
71.218
</td>
<td style="text-align:right;">
9230783
</td>
<td style="text-align:right;">
15870.879
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
71.659
</td>
<td style="text-align:right;">
9826397
</td>
<td style="text-align:right;">
9325.068
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
72.232
</td>
<td style="text-align:right;">
10336594
</td>
<td style="text-align:right;">
7914.320
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
73.213
</td>
<td style="text-align:right;">
10111559
</td>
<td style="text-align:right;">
7236.075
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
74.002
</td>
<td style="text-align:right;">
10150265
</td>
<td style="text-align:right;">
9786.535
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
64.940
</td>
<td style="text-align:right;">
28549870
</td>
<td style="text-align:right;">
3834.035
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
66.660
</td>
<td style="text-align:right;">
29841614
</td>
<td style="text-align:right;">
4564.802
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.690
</td>
<td style="text-align:right;">
31158061
</td>
<td style="text-align:right;">
5693.844
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.440
</td>
<td style="text-align:right;">
32850275
</td>
<td style="text-align:right;">
7993.512
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
73.060
</td>
<td style="text-align:right;">
34513161
</td>
<td style="text-align:right;">
10638.751
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
74.390
</td>
<td style="text-align:right;">
36439000
</td>
<td style="text-align:right;">
13236.921
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.300
</td>
<td style="text-align:right;">
37983310
</td>
<td style="text-align:right;">
13926.170
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.900
</td>
<td style="text-align:right;">
38880702
</td>
<td style="text-align:right;">
15764.983
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.570
</td>
<td style="text-align:right;">
39549438
</td>
<td style="text-align:right;">
18603.065
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.770
</td>
<td style="text-align:right;">
39855442
</td>
<td style="text-align:right;">
20445.299
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.780
</td>
<td style="text-align:right;">
40152517
</td>
<td style="text-align:right;">
24835.472
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.941
</td>
<td style="text-align:right;">
40448191
</td>
<td style="text-align:right;">
28821.064
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
71.860
</td>
<td style="text-align:right;">
7124673
</td>
<td style="text-align:right;">
8527.845
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
72.490
</td>
<td style="text-align:right;">
7363802
</td>
<td style="text-align:right;">
9911.878
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
73.370
</td>
<td style="text-align:right;">
7561588
</td>
<td style="text-align:right;">
12329.442
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
74.160
</td>
<td style="text-align:right;">
7867931
</td>
<td style="text-align:right;">
15258.297
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
74.720
</td>
<td style="text-align:right;">
8122293
</td>
<td style="text-align:right;">
17832.025
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
75.440
</td>
<td style="text-align:right;">
8251648
</td>
<td style="text-align:right;">
18855.725
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
8325260
</td>
<td style="text-align:right;">
20667.381
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
77.190
</td>
<td style="text-align:right;">
8421403
</td>
<td style="text-align:right;">
23586.929
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
78.160
</td>
<td style="text-align:right;">
8718867
</td>
<td style="text-align:right;">
23880.017
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
79.390
</td>
<td style="text-align:right;">
8897619
</td>
<td style="text-align:right;">
25266.595
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.040
</td>
<td style="text-align:right;">
8954175
</td>
<td style="text-align:right;">
29341.631
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.884
</td>
<td style="text-align:right;">
9031088
</td>
<td style="text-align:right;">
33859.748
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
69.180
</td>
<td style="text-align:right;">
50430000
</td>
<td style="text-align:right;">
9979.508
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
70.420
</td>
<td style="text-align:right;">
51430000
</td>
<td style="text-align:right;">
11283.178
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.760
</td>
<td style="text-align:right;">
53292000
</td>
<td style="text-align:right;">
12477.177
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.360
</td>
<td style="text-align:right;">
54959000
</td>
<td style="text-align:right;">
14142.851
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.010
</td>
<td style="text-align:right;">
56079000
</td>
<td style="text-align:right;">
15895.116
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.760
</td>
<td style="text-align:right;">
56179000
</td>
<td style="text-align:right;">
17428.748
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.040
</td>
<td style="text-align:right;">
56339704
</td>
<td style="text-align:right;">
18232.425
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
75.007
</td>
<td style="text-align:right;">
56981620
</td>
<td style="text-align:right;">
21664.788
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
57866349
</td>
<td style="text-align:right;">
22705.093
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.218
</td>
<td style="text-align:right;">
58808266
</td>
<td style="text-align:right;">
26074.531
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.471
</td>
<td style="text-align:right;">
59912431
</td>
<td style="text-align:right;">
29478.999
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.425
</td>
<td style="text-align:right;">
60776238
</td>
<td style="text-align:right;">
33203.261
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
</tbody>
</table>

"left\_join" preserves the columns and rows of the first data frame, while matching up new columns from the second data frame

##### right\_join

``` r
kable(right_join(Old_DF, New_DF), format="html") %>%
  kable_styling() %>%
  scroll_box(width = "1000px", height = "500px")
```

    ## Joining, by = "country"

    ## Warning: Column `country` joining factors with different levels, coercing
    ## to character vector

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:left;">
continent
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
lifeExp
</th>
<th style="text-align:right;">
pop
</th>
<th style="text-align:right;">
gdpPercap
</th>
<th style="text-align:left;">
Language
</th>
<th style="text-align:left;">
Capital
</th>
<th style="text-align:left;">
NATO\_Member
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
69.180
</td>
<td style="text-align:right;">
50430000
</td>
<td style="text-align:right;">
9979.508
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
70.420
</td>
<td style="text-align:right;">
51430000
</td>
<td style="text-align:right;">
11283.178
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.760
</td>
<td style="text-align:right;">
53292000
</td>
<td style="text-align:right;">
12477.177
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.360
</td>
<td style="text-align:right;">
54959000
</td>
<td style="text-align:right;">
14142.851
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.010
</td>
<td style="text-align:right;">
56079000
</td>
<td style="text-align:right;">
15895.116
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.760
</td>
<td style="text-align:right;">
56179000
</td>
<td style="text-align:right;">
17428.748
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.040
</td>
<td style="text-align:right;">
56339704
</td>
<td style="text-align:right;">
18232.425
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
75.007
</td>
<td style="text-align:right;">
56981620
</td>
<td style="text-align:right;">
21664.788
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
57866349
</td>
<td style="text-align:right;">
22705.093
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.218
</td>
<td style="text-align:right;">
58808266
</td>
<td style="text-align:right;">
26074.531
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.471
</td>
<td style="text-align:right;">
59912431
</td>
<td style="text-align:right;">
29478.999
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.425
</td>
<td style="text-align:right;">
60776238
</td>
<td style="text-align:right;">
33203.261
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
64.940
</td>
<td style="text-align:right;">
28549870
</td>
<td style="text-align:right;">
3834.035
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
66.660
</td>
<td style="text-align:right;">
29841614
</td>
<td style="text-align:right;">
4564.802
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.690
</td>
<td style="text-align:right;">
31158061
</td>
<td style="text-align:right;">
5693.844
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.440
</td>
<td style="text-align:right;">
32850275
</td>
<td style="text-align:right;">
7993.512
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
73.060
</td>
<td style="text-align:right;">
34513161
</td>
<td style="text-align:right;">
10638.751
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
74.390
</td>
<td style="text-align:right;">
36439000
</td>
<td style="text-align:right;">
13236.921
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.300
</td>
<td style="text-align:right;">
37983310
</td>
<td style="text-align:right;">
13926.170
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.900
</td>
<td style="text-align:right;">
38880702
</td>
<td style="text-align:right;">
15764.983
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.570
</td>
<td style="text-align:right;">
39549438
</td>
<td style="text-align:right;">
18603.065
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.770
</td>
<td style="text-align:right;">
39855442
</td>
<td style="text-align:right;">
20445.299
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.780
</td>
<td style="text-align:right;">
40152517
</td>
<td style="text-align:right;">
24835.472
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.941
</td>
<td style="text-align:right;">
40448191
</td>
<td style="text-align:right;">
28821.064
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.410
</td>
<td style="text-align:right;">
42459667
</td>
<td style="text-align:right;">
7029.809
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
68.930
</td>
<td style="text-align:right;">
44310863
</td>
<td style="text-align:right;">
8662.835
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.510
</td>
<td style="text-align:right;">
47124000
</td>
<td style="text-align:right;">
10560.486
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.550
</td>
<td style="text-align:right;">
49569000
</td>
<td style="text-align:right;">
12999.918
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.380
</td>
<td style="text-align:right;">
51732000
</td>
<td style="text-align:right;">
16107.192
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.830
</td>
<td style="text-align:right;">
53165019
</td>
<td style="text-align:right;">
18292.635
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.890
</td>
<td style="text-align:right;">
54433565
</td>
<td style="text-align:right;">
20293.897
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.340
</td>
<td style="text-align:right;">
55630100
</td>
<td style="text-align:right;">
22066.442
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.460
</td>
<td style="text-align:right;">
57374179
</td>
<td style="text-align:right;">
24703.796
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.640
</td>
<td style="text-align:right;">
58623428
</td>
<td style="text-align:right;">
25889.785
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.590
</td>
<td style="text-align:right;">
59925035
</td>
<td style="text-align:right;">
28926.032
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.657
</td>
<td style="text-align:right;">
61083916
</td>
<td style="text-align:right;">
30470.017
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.500
</td>
<td style="text-align:right;">
69145952
</td>
<td style="text-align:right;">
7144.114
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
69.100
</td>
<td style="text-align:right;">
71019069
</td>
<td style="text-align:right;">
10187.827
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
73739117
</td>
<td style="text-align:right;">
12902.463
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
70.800
</td>
<td style="text-align:right;">
76368453
</td>
<td style="text-align:right;">
14745.626
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
71.000
</td>
<td style="text-align:right;">
78717088
</td>
<td style="text-align:right;">
18016.180
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.500
</td>
<td style="text-align:right;">
78160773
</td>
<td style="text-align:right;">
20512.921
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
73.800
</td>
<td style="text-align:right;">
78335266
</td>
<td style="text-align:right;">
22031.533
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
74.847
</td>
<td style="text-align:right;">
77718298
</td>
<td style="text-align:right;">
24639.186
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.070
</td>
<td style="text-align:right;">
80597764
</td>
<td style="text-align:right;">
26505.303
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.340
</td>
<td style="text-align:right;">
82011073
</td>
<td style="text-align:right;">
27788.884
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.670
</td>
<td style="text-align:right;">
82350671
</td>
<td style="text-align:right;">
30035.802
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.406
</td>
<td style="text-align:right;">
82400996
</td>
<td style="text-align:right;">
32170.374
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
57.996
</td>
<td style="text-align:right;">
6860147
</td>
<td style="text-align:right;">
3581.459
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
61.685
</td>
<td style="text-align:right;">
7271135
</td>
<td style="text-align:right;">
4981.091
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
64.531
</td>
<td style="text-align:right;">
7616060
</td>
<td style="text-align:right;">
6289.629
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
66.914
</td>
<td style="text-align:right;">
7971222
</td>
<td style="text-align:right;">
7991.707
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
68.700
</td>
<td style="text-align:right;">
8313288
</td>
<td style="text-align:right;">
10522.067
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
8686367
</td>
<td style="text-align:right;">
12980.670
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
70.162
</td>
<td style="text-align:right;">
9032824
</td>
<td style="text-align:right;">
15181.093
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
71.218
</td>
<td style="text-align:right;">
9230783
</td>
<td style="text-align:right;">
15870.879
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
71.659
</td>
<td style="text-align:right;">
9826397
</td>
<td style="text-align:right;">
9325.068
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
72.232
</td>
<td style="text-align:right;">
10336594
</td>
<td style="text-align:right;">
7914.320
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
73.213
</td>
<td style="text-align:right;">
10111559
</td>
<td style="text-align:right;">
7236.075
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
74.002
</td>
<td style="text-align:right;">
10150265
</td>
<td style="text-align:right;">
9786.535
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
65.940
</td>
<td style="text-align:right;">
47666000
</td>
<td style="text-align:right;">
4931.404
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
67.810
</td>
<td style="text-align:right;">
49182000
</td>
<td style="text-align:right;">
6248.656
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.240
</td>
<td style="text-align:right;">
50843200
</td>
<td style="text-align:right;">
8243.582
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.060
</td>
<td style="text-align:right;">
52667100
</td>
<td style="text-align:right;">
10022.401
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.190
</td>
<td style="text-align:right;">
54365564
</td>
<td style="text-align:right;">
12269.274
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.480
</td>
<td style="text-align:right;">
56059245
</td>
<td style="text-align:right;">
14255.985
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.980
</td>
<td style="text-align:right;">
56535636
</td>
<td style="text-align:right;">
16537.483
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
56729703
</td>
<td style="text-align:right;">
19207.235
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.440
</td>
<td style="text-align:right;">
56840847
</td>
<td style="text-align:right;">
22013.645
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.820
</td>
<td style="text-align:right;">
57479469
</td>
<td style="text-align:right;">
24675.024
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.240
</td>
<td style="text-align:right;">
57926999
</td>
<td style="text-align:right;">
27968.098
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.546
</td>
<td style="text-align:right;">
58147733
</td>
<td style="text-align:right;">
28569.720
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
71.860
</td>
<td style="text-align:right;">
7124673
</td>
<td style="text-align:right;">
8527.845
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
72.490
</td>
<td style="text-align:right;">
7363802
</td>
<td style="text-align:right;">
9911.878
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
73.370
</td>
<td style="text-align:right;">
7561588
</td>
<td style="text-align:right;">
12329.442
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
74.160
</td>
<td style="text-align:right;">
7867931
</td>
<td style="text-align:right;">
15258.297
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
74.720
</td>
<td style="text-align:right;">
8122293
</td>
<td style="text-align:right;">
17832.025
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
75.440
</td>
<td style="text-align:right;">
8251648
</td>
<td style="text-align:right;">
18855.725
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
8325260
</td>
<td style="text-align:right;">
20667.381
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
77.190
</td>
<td style="text-align:right;">
8421403
</td>
<td style="text-align:right;">
23586.929
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
78.160
</td>
<td style="text-align:right;">
8718867
</td>
<td style="text-align:right;">
23880.017
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
79.390
</td>
<td style="text-align:right;">
8897619
</td>
<td style="text-align:right;">
25266.595
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.040
</td>
<td style="text-align:right;">
8954175
</td>
<td style="text-align:right;">
29341.631
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.884
</td>
<td style="text-align:right;">
9031088
</td>
<td style="text-align:right;">
33859.748
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
</tbody>
</table>

"right\_join" preserves the columns and rows of the second data frame, while matching up new columns from the first data frame

##### inner\_join

``` r
kable(inner_join(Old_DF, New_DF), format="html") %>%
  kable_styling() %>%
  scroll_box(width = "1000px", height = "500px")
```

    ## Joining, by = "country"

    ## Warning: Column `country` joining factors with different levels, coercing
    ## to character vector

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:left;">
continent
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
lifeExp
</th>
<th style="text-align:right;">
pop
</th>
<th style="text-align:right;">
gdpPercap
</th>
<th style="text-align:left;">
Language
</th>
<th style="text-align:left;">
Capital
</th>
<th style="text-align:left;">
NATO\_Member
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.410
</td>
<td style="text-align:right;">
42459667
</td>
<td style="text-align:right;">
7029.809
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
68.930
</td>
<td style="text-align:right;">
44310863
</td>
<td style="text-align:right;">
8662.835
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.510
</td>
<td style="text-align:right;">
47124000
</td>
<td style="text-align:right;">
10560.486
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.550
</td>
<td style="text-align:right;">
49569000
</td>
<td style="text-align:right;">
12999.918
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.380
</td>
<td style="text-align:right;">
51732000
</td>
<td style="text-align:right;">
16107.192
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.830
</td>
<td style="text-align:right;">
53165019
</td>
<td style="text-align:right;">
18292.635
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.890
</td>
<td style="text-align:right;">
54433565
</td>
<td style="text-align:right;">
20293.897
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.340
</td>
<td style="text-align:right;">
55630100
</td>
<td style="text-align:right;">
22066.442
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.460
</td>
<td style="text-align:right;">
57374179
</td>
<td style="text-align:right;">
24703.796
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.640
</td>
<td style="text-align:right;">
58623428
</td>
<td style="text-align:right;">
25889.785
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.590
</td>
<td style="text-align:right;">
59925035
</td>
<td style="text-align:right;">
28926.032
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.657
</td>
<td style="text-align:right;">
61083916
</td>
<td style="text-align:right;">
30470.017
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.500
</td>
<td style="text-align:right;">
69145952
</td>
<td style="text-align:right;">
7144.114
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
69.100
</td>
<td style="text-align:right;">
71019069
</td>
<td style="text-align:right;">
10187.827
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
73739117
</td>
<td style="text-align:right;">
12902.463
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
70.800
</td>
<td style="text-align:right;">
76368453
</td>
<td style="text-align:right;">
14745.626
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
71.000
</td>
<td style="text-align:right;">
78717088
</td>
<td style="text-align:right;">
18016.180
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.500
</td>
<td style="text-align:right;">
78160773
</td>
<td style="text-align:right;">
20512.921
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
73.800
</td>
<td style="text-align:right;">
78335266
</td>
<td style="text-align:right;">
22031.533
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
74.847
</td>
<td style="text-align:right;">
77718298
</td>
<td style="text-align:right;">
24639.186
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.070
</td>
<td style="text-align:right;">
80597764
</td>
<td style="text-align:right;">
26505.303
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.340
</td>
<td style="text-align:right;">
82011073
</td>
<td style="text-align:right;">
27788.884
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.670
</td>
<td style="text-align:right;">
82350671
</td>
<td style="text-align:right;">
30035.802
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.406
</td>
<td style="text-align:right;">
82400996
</td>
<td style="text-align:right;">
32170.374
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
65.940
</td>
<td style="text-align:right;">
47666000
</td>
<td style="text-align:right;">
4931.404
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
67.810
</td>
<td style="text-align:right;">
49182000
</td>
<td style="text-align:right;">
6248.656
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.240
</td>
<td style="text-align:right;">
50843200
</td>
<td style="text-align:right;">
8243.582
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.060
</td>
<td style="text-align:right;">
52667100
</td>
<td style="text-align:right;">
10022.401
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.190
</td>
<td style="text-align:right;">
54365564
</td>
<td style="text-align:right;">
12269.274
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.480
</td>
<td style="text-align:right;">
56059245
</td>
<td style="text-align:right;">
14255.985
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.980
</td>
<td style="text-align:right;">
56535636
</td>
<td style="text-align:right;">
16537.483
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
56729703
</td>
<td style="text-align:right;">
19207.235
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.440
</td>
<td style="text-align:right;">
56840847
</td>
<td style="text-align:right;">
22013.645
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.820
</td>
<td style="text-align:right;">
57479469
</td>
<td style="text-align:right;">
24675.024
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.240
</td>
<td style="text-align:right;">
57926999
</td>
<td style="text-align:right;">
27968.098
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.546
</td>
<td style="text-align:right;">
58147733
</td>
<td style="text-align:right;">
28569.720
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
57.996
</td>
<td style="text-align:right;">
6860147
</td>
<td style="text-align:right;">
3581.459
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
61.685
</td>
<td style="text-align:right;">
7271135
</td>
<td style="text-align:right;">
4981.091
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
64.531
</td>
<td style="text-align:right;">
7616060
</td>
<td style="text-align:right;">
6289.629
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
66.914
</td>
<td style="text-align:right;">
7971222
</td>
<td style="text-align:right;">
7991.707
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
68.700
</td>
<td style="text-align:right;">
8313288
</td>
<td style="text-align:right;">
10522.067
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
8686367
</td>
<td style="text-align:right;">
12980.670
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
70.162
</td>
<td style="text-align:right;">
9032824
</td>
<td style="text-align:right;">
15181.093
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
71.218
</td>
<td style="text-align:right;">
9230783
</td>
<td style="text-align:right;">
15870.879
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
71.659
</td>
<td style="text-align:right;">
9826397
</td>
<td style="text-align:right;">
9325.068
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
72.232
</td>
<td style="text-align:right;">
10336594
</td>
<td style="text-align:right;">
7914.320
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
73.213
</td>
<td style="text-align:right;">
10111559
</td>
<td style="text-align:right;">
7236.075
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
74.002
</td>
<td style="text-align:right;">
10150265
</td>
<td style="text-align:right;">
9786.535
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
64.940
</td>
<td style="text-align:right;">
28549870
</td>
<td style="text-align:right;">
3834.035
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
66.660
</td>
<td style="text-align:right;">
29841614
</td>
<td style="text-align:right;">
4564.802
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.690
</td>
<td style="text-align:right;">
31158061
</td>
<td style="text-align:right;">
5693.844
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.440
</td>
<td style="text-align:right;">
32850275
</td>
<td style="text-align:right;">
7993.512
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
73.060
</td>
<td style="text-align:right;">
34513161
</td>
<td style="text-align:right;">
10638.751
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
74.390
</td>
<td style="text-align:right;">
36439000
</td>
<td style="text-align:right;">
13236.921
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.300
</td>
<td style="text-align:right;">
37983310
</td>
<td style="text-align:right;">
13926.170
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.900
</td>
<td style="text-align:right;">
38880702
</td>
<td style="text-align:right;">
15764.983
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.570
</td>
<td style="text-align:right;">
39549438
</td>
<td style="text-align:right;">
18603.065
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.770
</td>
<td style="text-align:right;">
39855442
</td>
<td style="text-align:right;">
20445.299
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.780
</td>
<td style="text-align:right;">
40152517
</td>
<td style="text-align:right;">
24835.472
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.941
</td>
<td style="text-align:right;">
40448191
</td>
<td style="text-align:right;">
28821.064
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
71.860
</td>
<td style="text-align:right;">
7124673
</td>
<td style="text-align:right;">
8527.845
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
72.490
</td>
<td style="text-align:right;">
7363802
</td>
<td style="text-align:right;">
9911.878
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
73.370
</td>
<td style="text-align:right;">
7561588
</td>
<td style="text-align:right;">
12329.442
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
74.160
</td>
<td style="text-align:right;">
7867931
</td>
<td style="text-align:right;">
15258.297
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
74.720
</td>
<td style="text-align:right;">
8122293
</td>
<td style="text-align:right;">
17832.025
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
75.440
</td>
<td style="text-align:right;">
8251648
</td>
<td style="text-align:right;">
18855.725
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
8325260
</td>
<td style="text-align:right;">
20667.381
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
77.190
</td>
<td style="text-align:right;">
8421403
</td>
<td style="text-align:right;">
23586.929
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
78.160
</td>
<td style="text-align:right;">
8718867
</td>
<td style="text-align:right;">
23880.017
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
79.390
</td>
<td style="text-align:right;">
8897619
</td>
<td style="text-align:right;">
25266.595
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.040
</td>
<td style="text-align:right;">
8954175
</td>
<td style="text-align:right;">
29341.631
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.884
</td>
<td style="text-align:right;">
9031088
</td>
<td style="text-align:right;">
33859.748
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
69.180
</td>
<td style="text-align:right;">
50430000
</td>
<td style="text-align:right;">
9979.508
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
70.420
</td>
<td style="text-align:right;">
51430000
</td>
<td style="text-align:right;">
11283.178
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.760
</td>
<td style="text-align:right;">
53292000
</td>
<td style="text-align:right;">
12477.177
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.360
</td>
<td style="text-align:right;">
54959000
</td>
<td style="text-align:right;">
14142.851
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.010
</td>
<td style="text-align:right;">
56079000
</td>
<td style="text-align:right;">
15895.116
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.760
</td>
<td style="text-align:right;">
56179000
</td>
<td style="text-align:right;">
17428.748
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.040
</td>
<td style="text-align:right;">
56339704
</td>
<td style="text-align:right;">
18232.425
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
75.007
</td>
<td style="text-align:right;">
56981620
</td>
<td style="text-align:right;">
21664.788
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
57866349
</td>
<td style="text-align:right;">
22705.093
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.218
</td>
<td style="text-align:right;">
58808266
</td>
<td style="text-align:right;">
26074.531
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.471
</td>
<td style="text-align:right;">
59912431
</td>
<td style="text-align:right;">
29478.999
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.425
</td>
<td style="text-align:right;">
60776238
</td>
<td style="text-align:right;">
33203.261
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
</tbody>
</table>

"inner\_join" preserves the columns between both data frames, but only keeps rows that have matching values between the data frames (kind of like an intersection).

##### full\_join

``` r
kable(full_join(Old_DF, New_DF), format="html") %>%
  kable_styling() %>%
  scroll_box(width = "1000px", height = "500px")
```

    ## Joining, by = "country"

    ## Warning: Column `country` joining factors with different levels, coercing
    ## to character vector

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:left;">
continent
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
lifeExp
</th>
<th style="text-align:right;">
pop
</th>
<th style="text-align:right;">
gdpPercap
</th>
<th style="text-align:left;">
Language
</th>
<th style="text-align:left;">
Capital
</th>
<th style="text-align:left;">
NATO\_Member
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.410
</td>
<td style="text-align:right;">
42459667
</td>
<td style="text-align:right;">
7029.809
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
68.930
</td>
<td style="text-align:right;">
44310863
</td>
<td style="text-align:right;">
8662.835
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.510
</td>
<td style="text-align:right;">
47124000
</td>
<td style="text-align:right;">
10560.486
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.550
</td>
<td style="text-align:right;">
49569000
</td>
<td style="text-align:right;">
12999.918
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.380
</td>
<td style="text-align:right;">
51732000
</td>
<td style="text-align:right;">
16107.192
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.830
</td>
<td style="text-align:right;">
53165019
</td>
<td style="text-align:right;">
18292.635
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.890
</td>
<td style="text-align:right;">
54433565
</td>
<td style="text-align:right;">
20293.897
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.340
</td>
<td style="text-align:right;">
55630100
</td>
<td style="text-align:right;">
22066.442
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.460
</td>
<td style="text-align:right;">
57374179
</td>
<td style="text-align:right;">
24703.796
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.640
</td>
<td style="text-align:right;">
58623428
</td>
<td style="text-align:right;">
25889.785
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.590
</td>
<td style="text-align:right;">
59925035
</td>
<td style="text-align:right;">
28926.032
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.657
</td>
<td style="text-align:right;">
61083916
</td>
<td style="text-align:right;">
30470.017
</td>
<td style="text-align:left;">
French
</td>
<td style="text-align:left;">
Paris
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.500
</td>
<td style="text-align:right;">
69145952
</td>
<td style="text-align:right;">
7144.114
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
69.100
</td>
<td style="text-align:right;">
71019069
</td>
<td style="text-align:right;">
10187.827
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
73739117
</td>
<td style="text-align:right;">
12902.463
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
70.800
</td>
<td style="text-align:right;">
76368453
</td>
<td style="text-align:right;">
14745.626
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
71.000
</td>
<td style="text-align:right;">
78717088
</td>
<td style="text-align:right;">
18016.180
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.500
</td>
<td style="text-align:right;">
78160773
</td>
<td style="text-align:right;">
20512.921
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
73.800
</td>
<td style="text-align:right;">
78335266
</td>
<td style="text-align:right;">
22031.533
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
74.847
</td>
<td style="text-align:right;">
77718298
</td>
<td style="text-align:right;">
24639.186
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.070
</td>
<td style="text-align:right;">
80597764
</td>
<td style="text-align:right;">
26505.303
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.340
</td>
<td style="text-align:right;">
82011073
</td>
<td style="text-align:right;">
27788.884
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.670
</td>
<td style="text-align:right;">
82350671
</td>
<td style="text-align:right;">
30035.802
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.406
</td>
<td style="text-align:right;">
82400996
</td>
<td style="text-align:right;">
32170.374
</td>
<td style="text-align:left;">
German
</td>
<td style="text-align:left;">
Berlin
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
65.940
</td>
<td style="text-align:right;">
47666000
</td>
<td style="text-align:right;">
4931.404
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
67.810
</td>
<td style="text-align:right;">
49182000
</td>
<td style="text-align:right;">
6248.656
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.240
</td>
<td style="text-align:right;">
50843200
</td>
<td style="text-align:right;">
8243.582
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.060
</td>
<td style="text-align:right;">
52667100
</td>
<td style="text-align:right;">
10022.401
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.190
</td>
<td style="text-align:right;">
54365564
</td>
<td style="text-align:right;">
12269.274
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.480
</td>
<td style="text-align:right;">
56059245
</td>
<td style="text-align:right;">
14255.985
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.980
</td>
<td style="text-align:right;">
56535636
</td>
<td style="text-align:right;">
16537.483
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
56729703
</td>
<td style="text-align:right;">
19207.235
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.440
</td>
<td style="text-align:right;">
56840847
</td>
<td style="text-align:right;">
22013.645
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.820
</td>
<td style="text-align:right;">
57479469
</td>
<td style="text-align:right;">
24675.024
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.240
</td>
<td style="text-align:right;">
57926999
</td>
<td style="text-align:right;">
27968.098
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.546
</td>
<td style="text-align:right;">
58147733
</td>
<td style="text-align:right;">
28569.720
</td>
<td style="text-align:left;">
Italian
</td>
<td style="text-align:left;">
Rome
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
57.996
</td>
<td style="text-align:right;">
6860147
</td>
<td style="text-align:right;">
3581.459
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
61.685
</td>
<td style="text-align:right;">
7271135
</td>
<td style="text-align:right;">
4981.091
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
64.531
</td>
<td style="text-align:right;">
7616060
</td>
<td style="text-align:right;">
6289.629
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
66.914
</td>
<td style="text-align:right;">
7971222
</td>
<td style="text-align:right;">
7991.707
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
68.700
</td>
<td style="text-align:right;">
8313288
</td>
<td style="text-align:right;">
10522.067
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
8686367
</td>
<td style="text-align:right;">
12980.670
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
70.162
</td>
<td style="text-align:right;">
9032824
</td>
<td style="text-align:right;">
15181.093
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
71.218
</td>
<td style="text-align:right;">
9230783
</td>
<td style="text-align:right;">
15870.879
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
71.659
</td>
<td style="text-align:right;">
9826397
</td>
<td style="text-align:right;">
9325.068
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
72.232
</td>
<td style="text-align:right;">
10336594
</td>
<td style="text-align:right;">
7914.320
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
73.213
</td>
<td style="text-align:right;">
10111559
</td>
<td style="text-align:right;">
7236.075
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
74.002
</td>
<td style="text-align:right;">
10150265
</td>
<td style="text-align:right;">
9786.535
</td>
<td style="text-align:left;">
Serbian
</td>
<td style="text-align:left;">
Belgrade
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
64.940
</td>
<td style="text-align:right;">
28549870
</td>
<td style="text-align:right;">
3834.035
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
66.660
</td>
<td style="text-align:right;">
29841614
</td>
<td style="text-align:right;">
4564.802
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.690
</td>
<td style="text-align:right;">
31158061
</td>
<td style="text-align:right;">
5693.844
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.440
</td>
<td style="text-align:right;">
32850275
</td>
<td style="text-align:right;">
7993.512
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
73.060
</td>
<td style="text-align:right;">
34513161
</td>
<td style="text-align:right;">
10638.751
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
74.390
</td>
<td style="text-align:right;">
36439000
</td>
<td style="text-align:right;">
13236.921
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.300
</td>
<td style="text-align:right;">
37983310
</td>
<td style="text-align:right;">
13926.170
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.900
</td>
<td style="text-align:right;">
38880702
</td>
<td style="text-align:right;">
15764.983
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.570
</td>
<td style="text-align:right;">
39549438
</td>
<td style="text-align:right;">
18603.065
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.770
</td>
<td style="text-align:right;">
39855442
</td>
<td style="text-align:right;">
20445.299
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.780
</td>
<td style="text-align:right;">
40152517
</td>
<td style="text-align:right;">
24835.472
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.941
</td>
<td style="text-align:right;">
40448191
</td>
<td style="text-align:right;">
28821.064
</td>
<td style="text-align:left;">
Spanish
</td>
<td style="text-align:left;">
Madrid
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
71.860
</td>
<td style="text-align:right;">
7124673
</td>
<td style="text-align:right;">
8527.845
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
72.490
</td>
<td style="text-align:right;">
7363802
</td>
<td style="text-align:right;">
9911.878
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
73.370
</td>
<td style="text-align:right;">
7561588
</td>
<td style="text-align:right;">
12329.442
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
74.160
</td>
<td style="text-align:right;">
7867931
</td>
<td style="text-align:right;">
15258.297
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
74.720
</td>
<td style="text-align:right;">
8122293
</td>
<td style="text-align:right;">
17832.025
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
75.440
</td>
<td style="text-align:right;">
8251648
</td>
<td style="text-align:right;">
18855.725
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
8325260
</td>
<td style="text-align:right;">
20667.381
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
77.190
</td>
<td style="text-align:right;">
8421403
</td>
<td style="text-align:right;">
23586.929
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
78.160
</td>
<td style="text-align:right;">
8718867
</td>
<td style="text-align:right;">
23880.017
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
79.390
</td>
<td style="text-align:right;">
8897619
</td>
<td style="text-align:right;">
25266.595
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.040
</td>
<td style="text-align:right;">
8954175
</td>
<td style="text-align:right;">
29341.631
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.884
</td>
<td style="text-align:right;">
9031088
</td>
<td style="text-align:right;">
33859.748
</td>
<td style="text-align:left;">
Swedish
</td>
<td style="text-align:left;">
Stockholm
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
69.180
</td>
<td style="text-align:right;">
50430000
</td>
<td style="text-align:right;">
9979.508
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
70.420
</td>
<td style="text-align:right;">
51430000
</td>
<td style="text-align:right;">
11283.178
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.760
</td>
<td style="text-align:right;">
53292000
</td>
<td style="text-align:right;">
12477.177
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.360
</td>
<td style="text-align:right;">
54959000
</td>
<td style="text-align:right;">
14142.851
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.010
</td>
<td style="text-align:right;">
56079000
</td>
<td style="text-align:right;">
15895.116
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.760
</td>
<td style="text-align:right;">
56179000
</td>
<td style="text-align:right;">
17428.748
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.040
</td>
<td style="text-align:right;">
56339704
</td>
<td style="text-align:right;">
18232.425
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
75.007
</td>
<td style="text-align:right;">
56981620
</td>
<td style="text-align:right;">
21664.788
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
57866349
</td>
<td style="text-align:right;">
22705.093
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.218
</td>
<td style="text-align:right;">
58808266
</td>
<td style="text-align:right;">
26074.531
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.471
</td>
<td style="text-align:right;">
59912431
</td>
<td style="text-align:right;">
29478.999
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.425
</td>
<td style="text-align:right;">
60776238
</td>
<td style="text-align:right;">
33203.261
</td>
<td style="text-align:left;">
English
</td>
<td style="text-align:left;">
London
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
</tbody>
</table>

"full\_join" preserves both the columns and rows between both data frames (kind of like a union)

##### semi\_join

``` r
kable(semi_join(Old_DF, New_DF), format="html") %>%
  kable_styling() %>%
  scroll_box(width = "1000px", height = "500px")
```

    ## Joining, by = "country"

    ## Warning: Column `country` joining factors with different levels, coercing
    ## to character vector

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:left;">
continent
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
lifeExp
</th>
<th style="text-align:right;">
pop
</th>
<th style="text-align:right;">
gdpPercap
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.410
</td>
<td style="text-align:right;">
42459667
</td>
<td style="text-align:right;">
7029.809
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
68.930
</td>
<td style="text-align:right;">
44310863
</td>
<td style="text-align:right;">
8662.835
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.510
</td>
<td style="text-align:right;">
47124000
</td>
<td style="text-align:right;">
10560.486
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.550
</td>
<td style="text-align:right;">
49569000
</td>
<td style="text-align:right;">
12999.918
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.380
</td>
<td style="text-align:right;">
51732000
</td>
<td style="text-align:right;">
16107.192
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.830
</td>
<td style="text-align:right;">
53165019
</td>
<td style="text-align:right;">
18292.635
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.890
</td>
<td style="text-align:right;">
54433565
</td>
<td style="text-align:right;">
20293.897
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.340
</td>
<td style="text-align:right;">
55630100
</td>
<td style="text-align:right;">
22066.442
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.460
</td>
<td style="text-align:right;">
57374179
</td>
<td style="text-align:right;">
24703.796
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.640
</td>
<td style="text-align:right;">
58623428
</td>
<td style="text-align:right;">
25889.785
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.590
</td>
<td style="text-align:right;">
59925035
</td>
<td style="text-align:right;">
28926.032
</td>
</tr>
<tr>
<td style="text-align:left;">
France
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.657
</td>
<td style="text-align:right;">
61083916
</td>
<td style="text-align:right;">
30470.017
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
67.500
</td>
<td style="text-align:right;">
69145952
</td>
<td style="text-align:right;">
7144.114
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
69.100
</td>
<td style="text-align:right;">
71019069
</td>
<td style="text-align:right;">
10187.827
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
73739117
</td>
<td style="text-align:right;">
12902.463
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
70.800
</td>
<td style="text-align:right;">
76368453
</td>
<td style="text-align:right;">
14745.626
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
71.000
</td>
<td style="text-align:right;">
78717088
</td>
<td style="text-align:right;">
18016.180
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.500
</td>
<td style="text-align:right;">
78160773
</td>
<td style="text-align:right;">
20512.921
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
73.800
</td>
<td style="text-align:right;">
78335266
</td>
<td style="text-align:right;">
22031.533
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
74.847
</td>
<td style="text-align:right;">
77718298
</td>
<td style="text-align:right;">
24639.186
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.070
</td>
<td style="text-align:right;">
80597764
</td>
<td style="text-align:right;">
26505.303
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.340
</td>
<td style="text-align:right;">
82011073
</td>
<td style="text-align:right;">
27788.884
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.670
</td>
<td style="text-align:right;">
82350671
</td>
<td style="text-align:right;">
30035.802
</td>
</tr>
<tr>
<td style="text-align:left;">
Germany
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.406
</td>
<td style="text-align:right;">
82400996
</td>
<td style="text-align:right;">
32170.374
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
65.940
</td>
<td style="text-align:right;">
47666000
</td>
<td style="text-align:right;">
4931.404
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
67.810
</td>
<td style="text-align:right;">
49182000
</td>
<td style="text-align:right;">
6248.656
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.240
</td>
<td style="text-align:right;">
50843200
</td>
<td style="text-align:right;">
8243.582
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.060
</td>
<td style="text-align:right;">
52667100
</td>
<td style="text-align:right;">
10022.401
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.190
</td>
<td style="text-align:right;">
54365564
</td>
<td style="text-align:right;">
12269.274
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
73.480
</td>
<td style="text-align:right;">
56059245
</td>
<td style="text-align:right;">
14255.985
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.980
</td>
<td style="text-align:right;">
56535636
</td>
<td style="text-align:right;">
16537.483
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
56729703
</td>
<td style="text-align:right;">
19207.235
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.440
</td>
<td style="text-align:right;">
56840847
</td>
<td style="text-align:right;">
22013.645
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.820
</td>
<td style="text-align:right;">
57479469
</td>
<td style="text-align:right;">
24675.024
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.240
</td>
<td style="text-align:right;">
57926999
</td>
<td style="text-align:right;">
27968.098
</td>
</tr>
<tr>
<td style="text-align:left;">
Italy
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.546
</td>
<td style="text-align:right;">
58147733
</td>
<td style="text-align:right;">
28569.720
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
57.996
</td>
<td style="text-align:right;">
6860147
</td>
<td style="text-align:right;">
3581.459
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
61.685
</td>
<td style="text-align:right;">
7271135
</td>
<td style="text-align:right;">
4981.091
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
64.531
</td>
<td style="text-align:right;">
7616060
</td>
<td style="text-align:right;">
6289.629
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
66.914
</td>
<td style="text-align:right;">
7971222
</td>
<td style="text-align:right;">
7991.707
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
68.700
</td>
<td style="text-align:right;">
8313288
</td>
<td style="text-align:right;">
10522.067
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
8686367
</td>
<td style="text-align:right;">
12980.670
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
70.162
</td>
<td style="text-align:right;">
9032824
</td>
<td style="text-align:right;">
15181.093
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
71.218
</td>
<td style="text-align:right;">
9230783
</td>
<td style="text-align:right;">
15870.879
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
71.659
</td>
<td style="text-align:right;">
9826397
</td>
<td style="text-align:right;">
9325.068
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
72.232
</td>
<td style="text-align:right;">
10336594
</td>
<td style="text-align:right;">
7914.320
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
73.213
</td>
<td style="text-align:right;">
10111559
</td>
<td style="text-align:right;">
7236.075
</td>
</tr>
<tr>
<td style="text-align:left;">
Serbia
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
74.002
</td>
<td style="text-align:right;">
10150265
</td>
<td style="text-align:right;">
9786.535
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
64.940
</td>
<td style="text-align:right;">
28549870
</td>
<td style="text-align:right;">
3834.035
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
66.660
</td>
<td style="text-align:right;">
29841614
</td>
<td style="text-align:right;">
4564.802
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
69.690
</td>
<td style="text-align:right;">
31158061
</td>
<td style="text-align:right;">
5693.844
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.440
</td>
<td style="text-align:right;">
32850275
</td>
<td style="text-align:right;">
7993.512
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
73.060
</td>
<td style="text-align:right;">
34513161
</td>
<td style="text-align:right;">
10638.751
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
74.390
</td>
<td style="text-align:right;">
36439000
</td>
<td style="text-align:right;">
13236.921
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.300
</td>
<td style="text-align:right;">
37983310
</td>
<td style="text-align:right;">
13926.170
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.900
</td>
<td style="text-align:right;">
38880702
</td>
<td style="text-align:right;">
15764.983
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.570
</td>
<td style="text-align:right;">
39549438
</td>
<td style="text-align:right;">
18603.065
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.770
</td>
<td style="text-align:right;">
39855442
</td>
<td style="text-align:right;">
20445.299
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.780
</td>
<td style="text-align:right;">
40152517
</td>
<td style="text-align:right;">
24835.472
</td>
</tr>
<tr>
<td style="text-align:left;">
Spain
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.941
</td>
<td style="text-align:right;">
40448191
</td>
<td style="text-align:right;">
28821.064
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
71.860
</td>
<td style="text-align:right;">
7124673
</td>
<td style="text-align:right;">
8527.845
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
72.490
</td>
<td style="text-align:right;">
7363802
</td>
<td style="text-align:right;">
9911.878
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
73.370
</td>
<td style="text-align:right;">
7561588
</td>
<td style="text-align:right;">
12329.442
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
74.160
</td>
<td style="text-align:right;">
7867931
</td>
<td style="text-align:right;">
15258.297
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
74.720
</td>
<td style="text-align:right;">
8122293
</td>
<td style="text-align:right;">
17832.025
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
75.440
</td>
<td style="text-align:right;">
8251648
</td>
<td style="text-align:right;">
18855.725
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
8325260
</td>
<td style="text-align:right;">
20667.381
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
77.190
</td>
<td style="text-align:right;">
8421403
</td>
<td style="text-align:right;">
23586.929
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
78.160
</td>
<td style="text-align:right;">
8718867
</td>
<td style="text-align:right;">
23880.017
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
79.390
</td>
<td style="text-align:right;">
8897619
</td>
<td style="text-align:right;">
25266.595
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
80.040
</td>
<td style="text-align:right;">
8954175
</td>
<td style="text-align:right;">
29341.631
</td>
</tr>
<tr>
<td style="text-align:left;">
Sweden
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.884
</td>
<td style="text-align:right;">
9031088
</td>
<td style="text-align:right;">
33859.748
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
69.180
</td>
<td style="text-align:right;">
50430000
</td>
<td style="text-align:right;">
9979.508
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
70.420
</td>
<td style="text-align:right;">
51430000
</td>
<td style="text-align:right;">
11283.178
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
70.760
</td>
<td style="text-align:right;">
53292000
</td>
<td style="text-align:right;">
12477.177
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
71.360
</td>
<td style="text-align:right;">
54959000
</td>
<td style="text-align:right;">
14142.851
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.010
</td>
<td style="text-align:right;">
56079000
</td>
<td style="text-align:right;">
15895.116
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
72.760
</td>
<td style="text-align:right;">
56179000
</td>
<td style="text-align:right;">
17428.748
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
74.040
</td>
<td style="text-align:right;">
56339704
</td>
<td style="text-align:right;">
18232.425
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
75.007
</td>
<td style="text-align:right;">
56981620
</td>
<td style="text-align:right;">
21664.788
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
76.420
</td>
<td style="text-align:right;">
57866349
</td>
<td style="text-align:right;">
22705.093
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
77.218
</td>
<td style="text-align:right;">
58808266
</td>
<td style="text-align:right;">
26074.531
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
78.471
</td>
<td style="text-align:right;">
59912431
</td>
<td style="text-align:right;">
29478.999
</td>
</tr>
<tr>
<td style="text-align:left;">
United Kingdom
</td>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
79.425
</td>
<td style="text-align:right;">
60776238
</td>
<td style="text-align:right;">
33203.261
</td>
</tr>
</tbody>
</table>

"semi\_join" preserves only the columns of the first data frame, and only keeps the specific rows of the first data frame if they match up with the second data frame

##### anti\_join

``` r
kable(anti_join(Old_DF, New_DF), format="html") %>%
  kable_styling() %>%
  scroll_box(width = "1000px", height = "500px")
```

    ## Joining, by = "country"

    ## Warning: Column `country` joining factors with different levels, coercing
    ## to character vector

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:left;">
continent
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
lifeExp
</th>
<th style="text-align:right;">
pop
</th>
<th style="text-align:right;">
gdpPercap
</th>
</tr>
</thead>
<tbody>
<tr>
</tr>
</tbody>
</table>

"anti\_join" preserves only the columns of the first data frame, and only keeps the specific rows of the first data frame if they DON'T match up with the second data frame

Activity 3: Looking at "merge" and "match"
------------------------------------------
