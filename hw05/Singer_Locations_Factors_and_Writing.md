Singer\_Locations\_Factors\_and\_Writing
================
Nikolas Krstic
October 13, 2017

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

``` r
suppressPackageStartupMessages(library(forcats))
```

    ## Warning: package 'forcats' was built under R version 3.4.2

``` r
suppressPackageStartupMessages(library(devtools))
```

    ## Warning: package 'devtools' was built under R version 3.4.2

``` r
suppressPackageStartupMessages(library(singer))
suppressPackageStartupMessages(library(forcats))
```

Factor Management
-----------------

``` r
data("singer_locations")
```

We're going to factorise on the following variables: artist\_name, name and city. This is because of the following information we have on the unique values of each of the categorical variables (that are not identifiers):

``` r
#10100
nrow(singer_locations)
```

    ## [1] 10100

``` r
#9799, too high and close to nrow value, not useful to factorise
length(unique(singer_locations$title))
```

    ## [1] 9799

``` r
#9049, too high again
length(unique(singer_locations$release))
```

    ## [1] 9049

``` r
#7498, acceptable because of absence of NAs
length(unique(singer_locations$artist_name))
```

    ## [1] 7498

``` r
#2913, good enough
length(unique(singer_locations$name))
```

    ## [1] 2913

``` r
#1317, good enough as well
length(unique(singer_locations$release))
```

    ## [1] 9049

#### Check for NA count

``` r
#5968
sum(is.na(singer_locations$name))
```

    ## [1] 5968

``` r
#0
sum(is.na(singer_locations$artist_name))
```

    ## [1] 0

``` r
#5971
sum(is.na(singer_locations$city))
```

    ## [1] 5971

### Factorisation

``` r
#Replace any NAs present with "Missing Information", to allow us to use as_factor
sl_new <- singer_locations %>%
  mutate(name = ifelse(is.na(singer_locations$name), "Missing Information", singer_locations$name), artist_name = ifelse(is.na(singer_locations$artist_name), "Missing Information", singer_locations$artist_name), city = ifelse(is.na(singer_locations$city), "Missing Information", singer_locations$city))

sl_new <- sl_new %>%
  mutate(name_factor = as_factor(sl_new$name), artist_name_factor = as_factor(sl_new$artist_name), city_factor = as_factor(sl_new$city))
```

It should be noted that as\_factor sets the levels in the order that they first appear in the data, while as.factor sets the levels in alphabetical order. A serious drawback to as\_factor though is that the NAs within a variable hinder its ability to factorise. This is not a problem with as.factor, and usually you will end up changing the order of factor levels, regardless of whether you use either function.

#### Name of specific singer

``` r
#Number of levels
nlevels(sl_new$name_factor)
```

    ## [1] 2913

``` r
#Some sample levels
head(levels(sl_new$name_factor), n=10)
```

    ##  [1] "Missing Information"      "Gene Chandler"           
    ##  [3] "Paul Horn"                "Dorothy Ashby"           
    ##  [5] "Barleyjuice"              "Madlib"                  
    ##  [7] "Seeed feat. Elephant Man" "Keali I Reichel"         
    ##  [9] "Little Feat"              "Joan Baez"

#### Artist name (can be band/singer/artist/etc.)

``` r
nlevels(sl_new$artist_name_factor)
```

    ## [1] 7498

``` r
head(levels(sl_new$artist_name_factor), n=10)
```

    ##  [1] "Motion City Soundtrack"         "Gene Chandler"                 
    ##  [3] "Paul Horn"                      "Ronnie Earl & the Broadcasters"
    ##  [5] "Dorothy Ashby"                  "Barleyjuice"                   
    ##  [7] "Vertigo Angels"                 "Wir Sind Helden"               
    ##  [9] "Simon & Garfunkel"              "Rabia Sorda"

#### City

``` r
nlevels(sl_new$city_factor)
```

    ## [1] 1317

``` r
head(levels(sl_new$city_factor), n=10)
```

    ##  [1] "Missing Information" "Chicago, IL"         "New York, NY"       
    ##  [4] "Detroit, MI"         "Pennsylvania"        "Oxnard, CA"         
    ##  [7] "Bonn"                "Hawaii"              "Los Angeles, CA"    
    ## [10] "Staten Island, NY"

### Removal of year 0 from dataset and drop factor levels

``` r
sl_new2 <- sl_new %>%
  filter(year != 0) %>%
  droplevels()

nrow(sl_new2)
```

    ## [1] 10000

We had 100 observations that were year 0. We examine how the factors have changed as well.

#### Name of specific singer

``` r
#Number of levels
nlevels(sl_new2$name_factor)
```

    ## [1] 2879

``` r
#Some sample levels
head(levels(sl_new2$name_factor), n=10)
```

    ##  [1] "Missing Information"      "Gene Chandler"           
    ##  [3] "Paul Horn"                "Dorothy Ashby"           
    ##  [5] "Barleyjuice"              "Madlib"                  
    ##  [7] "Seeed feat. Elephant Man" "Keali I Reichel"         
    ##  [9] "Little Feat"              "Joan Baez"

We have 34 levels that were dropped.

#### Artist name (can be band/singer/artist/etc.)

``` r
nlevels(sl_new2$artist_name_factor)
```

    ## [1] 7408

``` r
head(levels(sl_new2$artist_name_factor), n=10)
```

    ##  [1] "Motion City Soundtrack"         "Gene Chandler"                 
    ##  [3] "Paul Horn"                      "Ronnie Earl & the Broadcasters"
    ##  [5] "Dorothy Ashby"                  "Barleyjuice"                   
    ##  [7] "Vertigo Angels"                 "Wir Sind Helden"               
    ##  [9] "Simon & Garfunkel"              "Rabia Sorda"

We have 90 levels that were dropped.

#### City

``` r
nlevels(sl_new2$city_factor)
```

    ## [1] 1309

``` r
head(levels(sl_new2$city_factor), n=10)
```

    ##  [1] "Missing Information" "Chicago, IL"         "New York, NY"       
    ##  [4] "Detroit, MI"         "Pennsylvania"        "Oxnard, CA"         
    ##  [7] "Bonn"                "Hawaii"              "Los Angeles, CA"    
    ## [10] "Staten Island, NY"

We have 8 levels that were dropped.

### Reordering the levels of arist\_name\_factor by maximum artist\_familiarity

``` r
sl_new3 <- sl_new2 %>%
  mutate(artist_name_factor = fct_reorder(artist_name_factor, artist_familiarity, max))

head(levels(sl_new3$artist_name_factor), n=10)
```

    ##  [1] "Captain Capa"                        
    ##  [2] "Ella Washington"                     
    ##  [3] "Madness"                             
    ##  [4] "The (International) Noise Conspiracy"
    ##  [5] "Ludovico Einaudi & Ballaké Sissoko"  
    ##  [6] "Fight K5"                            
    ##  [7] "Antonio MacHin"                      
    ##  [8] "Retractor"                           
    ##  [9] "Cricco Castelli"                     
    ## [10] "Chuck Durfor"

Factor levels have been reordered.

### Writing and Reading to File

``` r
sl_new4 <- sl_new3 %>%
  mutate(city_factor = fct_reorder(city_factor, artist_familiarity, max)) %>%
  group_by(city_factor) %>%
  summarize(mean_artist_hotttnesss = mean(artist_hotttnesss))

head(levels(sl_new4$city_factor), n=10)
```

    ##  [1] "Darmstadt"               "namsos, NO"             
    ##  [3] "Memphis TN"              "New Hampshire"          
    ##  [5] "Chapel Hill NC"          "Duluth, MN"             
    ##  [7] "Algeria"                 "Bonn"                   
    ##  [9] "milwaulkee/atl/brooklyn" "Dougherty, OK"

``` r
write_csv(sl_new4, paste(getwd(), "/Data/Artist_Hotness_Data.csv", sep=""))
sl_new5 <- read_csv(paste(getwd(), "/Data/Artist_Hotness_Data.csv", sep=""))
```

    ## Parsed with column specification:
    ## cols(
    ##   city_factor = col_character(),
    ##   mean_artist_hotttnesss = col_double()
    ## )

``` r
str(sl_new5)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1309 obs. of  2 variables:
    ##  $ city_factor           : chr  "Darmstadt" "namsos, NO" "Memphis TN" "New Hampshire" ...
    ##  $ mean_artist_hotttnesss: num  0.496 0.375 0.328 0.374 0.244 ...
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 2
    ##   .. ..$ city_factor           : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ mean_artist_hotttnesss: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

It appears that the factor was not preserved with write\_csv

``` r
saveRDS(sl_new4, paste(getwd(), "/Data/Artist_Hotness_Data.csv", sep=""))
sl_new5 <- readRDS(paste(getwd(), "/Data/Artist_Hotness_Data.csv", sep=""))

str(sl_new5)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1309 obs. of  2 variables:
    ##  $ city_factor           : Factor w/ 1309 levels "Darmstadt","namsos, NO",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ mean_artist_hotttnesss: num  0.496 0.375 0.328 0.374 0.244 ...

``` r
head(levels(sl_new5$city_factor), n=10)
```

    ##  [1] "Darmstadt"               "namsos, NO"             
    ##  [3] "Memphis TN"              "New Hampshire"          
    ##  [5] "Chapel Hill NC"          "Duluth, MN"             
    ##  [7] "Algeria"                 "Bonn"                   
    ##  [9] "milwaulkee/atl/brooklyn" "Dougherty, OK"

It appears that the factor (and its reordered levels) were preserved with saveRDS

``` r
dput(sl_new4, paste(getwd(), "/Data/Artist_Hotness_Data.csv", sep=""))
sl_new5 <- dget(paste(getwd(), "/Data/Artist_Hotness_Data.csv", sep=""))

str(sl_new5)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1309 obs. of  2 variables:
    ##  $ city_factor           : Factor w/ 1309 levels "Darmstadt","namsos, NO",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ mean_artist_hotttnesss: num  0.496 0.375 0.328 0.374 0.244 ...

``` r
head(levels(sl_new5$city_factor), n=10)
```

    ##  [1] "Darmstadt"               "namsos, NO"             
    ##  [3] "Memphis TN"              "New Hampshire"          
    ##  [5] "Chapel Hill NC"          "Duluth, MN"             
    ##  [7] "Algeria"                 "Bonn"                   
    ##  [9] "milwaulkee/atl/brooklyn" "Dougherty, OK"

It appears that the factor (and its reordered levels) were preserved with dput

### Visualization Design

``` r
sl_new6 <- sl_new3 %>%
  group_by(city_factor) %>%
  summarize(mean_artist_familiarity = mean(artist_familiarity)) %>%
  mutate(city_factor = fct_reorder(city_factor, mean_artist_familiarity, max)) %>%
  filter(mean_artist_familiarity>0.85)

sl_new6 %>%
  ggplot(aes(x=city_factor, y=mean_artist_familiarity)) +
  theme_bw() +
  geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.4)) +
  labs(x="City", y="Artist Hotness", title="Cities with the Most Familiar Artists")
```

![](Singer_Locations_Factors_and_Writing_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

The factor along the x-axis appears to be ordered in the figure appropriately based on our reordering (based on maximum of mean artist familiarity).

``` r
sl_new7 <- sl_new3 %>%
  group_by(city_factor) %>%
  summarize(mean_artist_familiarity = mean(artist_familiarity)) %>%
  arrange(mean_artist_familiarity) %>%
  filter(mean_artist_familiarity>0.85)

sl_new7 %>%
  ggplot(aes(x=city_factor, y=mean_artist_familiarity)) +
  theme_bw() +
  geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.4)) +
  labs(x="City", y="Artist Hotness", title="Cities with the Most Familiar Artists")
```

![](Singer_Locations_Factors_and_Writing_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png)

However, in contrast, using only "arrange" to attempt to reorganize the appearance of the figure doesn't work

``` r
#Use the previous dataframe that had the reordered city factor
new_plot = sl_new6 %>%
  arrange(mean_artist_familiarity) %>%
  ggplot(aes(x=city_factor, y=mean_artist_familiarity)) +
  theme_bw() +
  geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.4)) +
  labs(x="City", y="Artist Hotness", title="Cities with the Most Familiar Artists")

new_plot
```

![](Singer_Locations_Factors_and_Writing_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-19-1.png)

Combining "arrange" with reordering of the factor does result in the same plot as just performing the reordering, so we can conclude that arranging the data has no effect on the plot.

### Writing Figures to File

``` r
ggsave(paste(getwd(), "/Plots/Mean_Artist_Familiarity_Plot.pdf", sep=""), plot=new_plot, height=6)
```

    ## Saving 7 x 6 in image

The embed doesn't seem to work, and I'm not entirely sure why. I would rather not put my local drive information in order to get this to work, however. You can find the plot in the corresponding folder, nevertheless

Saving our plot in the vector format (.pdf, .eps) because it provides the best resolution for graphs in general. Raster format is useful for photographs and other detailed visuals, but can potentially provide poorer resolution.

Went with most of the default options because they are often the best.

By not specifying the plot argument, ggsave will automatically save the last plot that was displayed, and therefore it's important to specify the plot you would like to save.

### But I want to do More:

``` r
sl_new8 <- sl_new3 %>%
  filter(city_factor %in% c(levels(city_factor)[1:10])) %>%
  droplevels() %>%
  mutate(state = fct_recode(city_factor, "NA" = "Missing Information", "Illinois" = "Chicago, IL", "New York" = "New York, NY", "Michigan" = "Detroit, MI", "California" = "Oxnard, CA", "California" = "Los Angeles, CA", "New York" = "Staten Island, NY"))

kable(head(sl_new8, n=30))
```

| track\_id          | title                            | song\_id           | release                             | artist\_id         | artist\_name                                |  year|  duration|  artist\_hotttnesss|  artist\_familiarity|  latitude|   longitude| name                     | city                | name\_factor             | artist\_name\_factor                        | city\_factor        | state        |
|:-------------------|:---------------------------------|:-------------------|:------------------------------------|:-------------------|:--------------------------------------------|-----:|---------:|-------------------:|--------------------:|---------:|-----------:|:-------------------------|:--------------------|:-------------------------|:--------------------------------------------|:--------------------|:-------------|
| TRWICRA128F42368DB | The Conversation (Cd)            | SOSURTI12A81C22FB8 | Even If It Kills Me                 | ARACDPV1187FB58DF4 | Motion City Soundtrack                      |  2007|  170.4485|           0.6410183|            0.8230522|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Motion City Soundtrack                      | Missing Information | NA           |
| TRXJANY128F42246FC | Lonely Island                    | SODESQP12A6D4F98EF | The Duke Of Earl                    | ARYBUAO1187FB3F4EB | Gene Chandler                               |  2004|  106.5530|           0.3937627|            0.5700167|  41.88415|   -87.63241| Gene Chandler            | Chicago, IL         | Gene Chandler            | Gene Chandler                               | Chicago, IL         | Illinois     |
| TRIKPCA128F424A553 | Here's That Rainy Day            | SOQUYQD12A8C131619 | Imprompture                         | AR4111G1187B9B58AB | Paul Horn                                   |  1998|  527.5947|           0.4306226|            0.5039940|  40.71455|   -74.00712| Paul Horn                | New York, NY        | Paul Horn                | Paul Horn                                   | New York, NY        | New York     |
| TRYEATD128F92F87C9 | Rego Park Blues                  | SOEZGRC12AB017F1AC | Still River                         | ARQDZP31187B98D623 | Ronnie Earl & the Broadcasters              |  1995|  695.1179|           0.3622792|            0.4773099|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Ronnie Earl & the Broadcasters              | Missing Information | NA           |
| TRBYYXH128F4264585 | Games                            | SOPIOCP12A8C13A322 | Afro-Harping                        | AR75GYU1187B9AE47A | Dorothy Ashby                               |  1968|  237.3220|           0.4107520|            0.5303468|  42.33168|   -83.04792| Dorothy Ashby            | Detroit, MI         | Dorothy Ashby            | Dorothy Ashby                               | Detroit, MI         | Michigan     |
| TRKFFKR128F9303AE3 | More Pipes                       | SOHQSPY12AB0181325 | Six Yanks                           | ARCENE01187B9AF929 | Barleyjuice                                 |  2006|  192.9400|           0.3762635|            0.5412950|  40.99471|   -77.60454| Barleyjuice              | Pennsylvania        | Barleyjuice              | Barleyjuice                                 | Pennsylvania        | Pennsylvania |
| TRSSNNI128F42661D8 | out OF my MIND                   | SOIHOMM12A8C139A02 | Eradicate Apathy                    | AR88PFB1187B9B834D | Vertigo Angels                              |  2003|  239.5424|           0.1814601|            0.2744329|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Vertigo Angels                              | Missing Information | NA           |
| TRXSSXI128F428FAD4 | Endlich Ein Grund Zur Panik      | SODTXQY12A6D4F748E | Soundso + Track-by-Track Kommentare | ARKQW4V1187B9AD20B | Wir Sind Helden                             |  2007|  222.8502|           0.4743547|            0.6979295|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Wir Sind Helden                             | Missing Information | NA           |
| TRDKANB128F4261BDE | I Am A Rock                      | SOCZZEQ12A8C1318A9 | Live From New York City\_ 1967      | ARIWB161187B9AA1D5 | Simon & Garfunkel                           |  1966|  177.9718|           0.5103250|            0.7964934|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Simon & Garfunkel                           | Missing Information | NA           |
| TRJQSXM128F932A05B | A perfect world                  | SOFJTIS12AB0184236 | Save Me From My Curse               | ARP3U2W1187FB4400E | Rabia Sorda                                 |  2006|  259.8395|           0.4196401|            0.5931096|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Rabia Sorda                                 | Missing Information | NA           |
| TRYEOWT128F934A8E1 | Suicide                          | SOVKSXG12AB018A6C1 | As Approved by the Committee        | ARVM9DU1187FB4E33C | The Telescopes                              |  1989|  476.4208|           0.3932899|            0.5890625|        NA|          NA| Missing Information      | Missing Information | Missing Information      | The Telescopes                              | Missing Information | NA           |
| TRLDWAR128F92EAFC3 | Erase                            | SOOBWOG12B34F1C7AD | Building 429                        | ARYK2ZC1187B99E0D9 | Building 429                                |  2008|  261.6420|           0.4953518|            0.7213940|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Building 429                                | Missing Information | NA           |
| TRXLXET128F425E4FD | Gravemakers & Gunslingers        | SOOSZAL12D02191FA1 | No World For Tomorrow               | ARIWKUS1187FB55354 | Coheed and Cambria                          |  2007|  260.6493|           0.6131468|            0.7952461|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Coheed and Cambria                          | Missing Information | NA           |
| TRXTNVI128F92D7FBD | Enemies                          | SOCYPBI12A8C143CDE | Enemies                             | ARRUFMQ11E2835E080 | In Case Of Fire                             |  2009|  176.6134|           0.3602394|            0.5514289|        NA|          NA| Missing Information      | Missing Information | Missing Information      | In Case Of Fire                             | Missing Information | NA           |
| TRPGGXE128F9322D0C | Draw The Line                    | SOJVVWZ12AB018312C | Evacuate The Dancefloor             | ARPD2KK1187B9B8B98 | Cascada                                     |  2009|  237.4526|           0.6243477|            0.8308743|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Cascada                                     | Missing Information | NA           |
| TRBUASZ128F4213085 | June 26th\_ 1999 - Purpose       | SOBXXMX12A6D4FB56F | The Taste Of Rain... Why Kneel      | AROGN2E1187B98D292 | Deep Puddle Dynamics                        |  1999|  206.3930|           0.3579857|            0.5071263|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Deep Puddle Dynamics                        | Missing Information | NA           |
| TRWKTVW12903CE5ACF | Indian Deli                      | SOGYBYQ12AB0188586 | Beat Konducta Vol. 3 & 4: In India  | AR17D2T1187FB4DBC2 | Madlib                                      |  2007|  107.7808|           0.5339732|            0.7640263|  34.20034|  -119.18044| Madlib                   | Oxnard, CA          | Madlib                   | Madlib                                      | Oxnard, CA          | California   |
| TRUWFXF128E0795D22 | Miss Gorgeous                    | SOTEIQB12A6702048D | Music Monks                         | ARDNZL61187B98F42D | Seeed's Pharaoh Riddim Feat. General Degree |  2003|  195.9702|           0.4800612|            0.3086738|  50.73230|     7.10169| Seeed feat. Elephant Man | Bonn                | Seeed feat. Elephant Man | Seeed's Pharaoh Riddim Feat. General Degree | Bonn                | Bonn         |
| TRUMIFU128F14B0E84 | Iron Tusk \[Live\]               | SOFJTZZ12A6D4FB13E | Colony Of Birchmen                  | ARMQHX71187B9890D3 | Mastodon                                    |  2007|  168.9334|           0.5742747|            0.7804617|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Mastodon                                    | Missing Information | NA           |
| TRYKVFW128F4243264 | Lahainaluna                      | SOUZVTG12A8C1308FB | Ke'alaokamaile                      | ARR9RE51187B98E899 | Keali'i Reichel                             |  2003|  245.7334|           0.3640586|            0.5649717|  19.59009|  -155.43414| Keali I Reichel          | Hawaii              | Keali I Reichel          | Keali'i Reichel                             | Hawaii              | Hawaii       |
| TRUNSOU12903CC52BD | The Ingenue (LP Version)         | SOJESNI12AB0186408 | Representing The Mambo              | AR6W5MS1187FB3BF47 | Little Feat                                 |  1989|  263.9669|           0.4791875|            0.6749188|  34.05349|  -118.24532| Little Feat              | Los Angeles, CA     | Little Feat              | Little Feat                                 | Los Angeles, CA     | California   |
| TRWPHSQ128F42AA08E | Chemistry                        | SORTJNE12A8C13DBE9 | I'm In The Mood For Dancing         | ARE3YH31187B994AAA | The Nolans                                  |  1982|  203.4934|           0.4340294|            0.5293624|        NA|          NA| Missing Information      | Missing Information | Missing Information      | The Nolans                                  | Missing Information | NA           |
| TRADKYY128F14900A6 | Don't Let Me Be Misunderstood    | SOKYDWX12A6D4FA455 | Acertei No Milenio                  | ARLFAHS11E2835E9FE | Angela Ro Ro                                |  2006|  335.8559|           0.3228789|            0.3785446|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Angela Ro Ro                                | Missing Information | NA           |
| TRBNFTT128F92FB599 | The Unquiet Grave (Child No. 78) | SOTSNHW12AB0182A9D | 5                                   | AR60ODO1187FB4D9AB | Joan Baez                                   |  1964|  261.4069|           0.4552892|            0.7322613|  40.57250|   -74.15400| Joan Baez                | Staten Island, NY   | Joan Baez                | Joan Baez                                   | Staten Island, NY   | New York     |
| TREGKZX128E0791970 | Down On You                      | SOJNQWD12A67020834 | Velveteen                           | AR241HA1187FB37B59 | Transvision Vamp                            |  1989|  262.2428|           0.4131472|            0.5276313|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Transvision Vamp                            | Missing Information | NA           |
| TRRNFHH128F92D262D | Savior                           | SOUJVIT12A8C1451C1 | Appeal To Reason                    | ARZWK2R1187B98F09F | Rise Against                                |  2008|  242.2591|           0.6250805|            0.8470817|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Rise Against                                | Missing Information | NA           |
| TRCSPAA128F92FA6A9 | One Way                          | SOGIOCP12AAFF406F8 | In Another Land                     | ARMK4631187FB5CE4A | Larry Norman                                |  1972|  144.4044|           0.3510869|            0.5011028|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Larry Norman                                | Missing Information | NA           |
| TRIPLUO128F426B260 | Working Girl                     | SOESGMB12A8C13728D | Amor\_ Humor                        | AR2G8XO1187B9952A1 | Nacho Cano                                  |  1999|  183.7187|           0.4013662|            0.4717612|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Nacho Cano                                  | Missing Information | NA           |
| TRKVGZF128F42B15B7 | Weightlessness                   | SOLFQWP12A8C13E2D3 | Waiting For Clearance               | ARJPCKV1187FB4BEEE | The Freelance Hellraiser                    |  2006|  241.5538|           0.0000000|            0.5392862|        NA|          NA| Missing Information      | Missing Information | Missing Information      | The Freelance Hellraiser                    | Missing Information | NA           |
| TRLGENP128F428DCC4 | That I Would Be Good             | SOBCDKP12A8C13AA68 | Start                               | ARMPIH61187FB44227 | Sun Yan-Zi                                  |  2002|  260.2836|           0.3784138|            0.4750278|        NA|          NA| Missing Information      | Missing Information | Missing Information      | Sun Yan-Zi                                  | Missing Information | NA           |

Created the factor for the states of a handful of cities. May become tedious to have to recode every single factor level when generating the new factor.

### Report on Process

Encountered a few challenges, namely trying to embed an image within the report. I was unsuccessful and I'm not sure if it's required that I point to the file directly on my local drive (that doesn't make sense to me). However, it insists the file doesn't exist if I don't specify the full path (even with the work directory set to within this project)

Otherwise, I feel like I had a good grasp on how factors worked before this week, but now I have a better understanding of how to manipulate them with dplyr, which I was originally inexperienced with before this class.

Also, it seems that write\_csv is generally better than write.csv, which I used fairly often. What's good about it is that it doesn't write the row names as well. This is useful because consistent reading and writing without proper data cleaning would result in multiple columns of row names, which was annoying.
