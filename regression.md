regression, final project
================
Laura Lynch
12/02/2019

``` r
# set up
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(modelr)
library(purrr)
library(stringr)


knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
    fig.width = 10, 
  fig.height = 6,
  out.width = "90%"
)
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
theme_set(theme_minimal() + theme(legend.position = "bottom"))
```

``` r
drug_death=
  read_csv("./data/Accidental_Drug_Related_Deaths_2012-2018.csv") %>%
  janitor::clean_names() %>%
  separate(date, into = c("date", "time"), sep = 11) %>%
  select(-time) %>%
  separate(date, into = c("month", "day", "year"), sep = " , ", convert = TRUE) %>%
  drop_na(age, sex, race, residence_county, residence_state) %>%
  mutate(death_city_longlat = sub("\\).*", "", sub(".*\\(", "", death_city_geo))) %>%
  # Laura changed date to month
  separate(month, into = c("month", "day", "year"), sep = "/", convert = TRUE, remove = FALSE) %>%
  # Laura deleted:      date = as.Date(date, format = '%m/%d/%Y'), 
  mutate(
     death_city_longlat = sub("\\).*", "", sub(".*\\(", "", death_city_geo))  ) %>% 
  separate(col = death_city_longlat, into = c("death_city_lat", "death_city_long"), sep = "," ,remove = FALSE) %>%
  mutate(
      injury_city_longlat = sub("\\).*", "", sub(".*\\(", "", injury_city_geo))  ) %>% 
      separate(col = injury_city_longlat, into = c("injury_city_lat", "injury_city_long"), sep = "," ,remove = FALSE) %>%
  mutate(
      residence_city_longlat = sub("\\).*", "", sub(".*\\(", "", residence_city_geo))  ) %>% 
      separate(col = residence_city_longlat, into = c("residence_city_lat", "residence_city_long"), sep = "," ,remove = FALSE) %>%
  ##dropping pre-split variables for latitude and longitude, dropping date_type because it is uninformative for this analysis 
  select(-residence_city_longlat, -death_city_longlat, -injury_city_longlat, -residence_city_geo, -injury_city_geo, -death_city_geo, -mannerof_death, -date_type) %>%
  mutate(race = recode(race,
                       "Asian Indian" = "Asian",
                       "Asian, Other" = "Asian",
                       "Chinese" = "Asian",
                       "Hawaiian" = "Other"),
         race = na_if(race, "Unknown")) 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   Age = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
drug_death
```

    ## # A tibble: 3,503 x 44
    ##    id    month   day  year   age sex   race  residence_city
    ##    <chr> <int> <int> <dbl> <dbl> <chr> <chr> <chr>         
    ##  1 16-0…     3    13  2016    30 Fema… White SANDY HOOK    
    ##  2 16-0…     3    31  2016    23 Male  White RYE           
    ##  3 15-0…     5    14  2015    50 Male  White DANBURY       
    ##  4 16-0…     1    13  2016    29 Male  Black NEW HAVEN     
    ##  5 16-0…     1    30  2016    54 Male  White MIDDLETOWN    
    ##  6 16-0…    12    20  2016    32 Male  White WINDHAM       
    ##  7 15-0…     6     2  2015    39 Fema… White ANSONIA       
    ##  8 16-0…    10     7  2016    34 Male  White COS COB       
    ##  9 16-0…     7    16  2016    27 Fema… White STRATFORD     
    ## 10 17-0…    10    13  2017    25 Male  White MERIDEN       
    ## # … with 3,493 more rows, and 36 more variables: residence_county <chr>,
    ## #   residence_state <chr>, death_city <chr>, death_county <chr>,
    ## #   location <chr>, locationif_other <chr>, descriptionof_injury <chr>,
    ## #   injury_place <chr>, injury_city <chr>, injury_county <chr>,
    ## #   injury_state <chr>, cod <chr>, other_significan <chr>, heroin <chr>,
    ## #   cocaine <chr>, fentanyl <chr>, fentanyl_analogue <chr>,
    ## #   oxycodone <chr>, oxymorphone <chr>, ethanol <chr>, hydrocodone <chr>,
    ## #   benzodiazepine <chr>, methadone <chr>, amphet <chr>, tramad <chr>,
    ## #   morphine_not_heroin <chr>, hydromorphone <chr>, other <chr>,
    ## #   opiate_nos <chr>, any_opioid <chr>, death_city_lat <chr>,
    ## #   death_city_long <chr>, injury_city_lat <chr>, injury_city_long <chr>,
    ## #   residence_city_lat <chr>, residence_city_long <chr>

``` r
CT = c("LITCHFIELD", "HARTFORD", "TOLLAND", "NEW LONDON", "MIDDLESEX", "NEW HAVEN", "WINDHAM", "FAIRFIELD")

drug_death_bin =
  drug_death %>%
  # binarize outcome: death in hospital YES or NO
  mutate(death_location = ifelse(location == "Hospital", c(1), c(0))) %>%
  # narrow data set to relevent regression variables
  select(death_location, age, sex, race, residence_county) %>%
  # identify out of state counties
  mutate(new_county = ifelse(residence_county %in% CT,
                             str_to_lower(residence_county, locale = "en"),
                             "out of state")) %>%
  select(-residence_county) %>%
  drop_na(death_location, race) %>%
  # create factor variables for regression
  mutate(death_location = factor(death_location, levels = c(1, 0), labels = c("In hospital", "Outside hospital")),
        sex = factor(sex, labels = c("Male", "Female", "Unknown")),
        race = factor(race, labels = c("White", "Black", "Hispanic, White", "Asian", "Other", "Hispanic, Black")),
        new_county = factor(new_county, labels = c("litchfield", "hartford", "tolland", "new london", "middlesex", "new haven", "windham", "fairfield", "out of state"))
          ) 

drug_death_bin
```

    ## # A tibble: 3,480 x 5
    ##    death_location     age sex    race            new_county  
    ##    <fct>            <dbl> <fct>  <fct>           <fct>       
    ##  1 In hospital         30 Male   Hispanic, Black litchfield  
    ##  2 In hospital         23 Female Hispanic, Black windham     
    ##  3 Outside hospital    50 Female Hispanic, Black litchfield  
    ##  4 Outside hospital    29 Female Black           middlesex   
    ##  5 Outside hospital    54 Female Hispanic, Black new london  
    ##  6 Outside hospital    32 Female Hispanic, Black out of state
    ##  7 In hospital         39 Male   Hispanic, Black middlesex   
    ##  8 Outside hospital    34 Female Hispanic, Black litchfield  
    ##  9 Outside hospital    27 Male   Hispanic, Black litchfield  
    ## 10 In hospital         25 Female Hispanic, Black middlesex   
    ## # … with 3,470 more rows

``` r
# specify predictor variable types
drug_death_bin %>%  
  mutate(
    age = as.numeric(age),
    sex = fct_infreq(sex),
    race = fct_infreq(race),
    new_county = fct_infreq(new_county)
    )
```

    ## # A tibble: 3,480 x 5
    ##    death_location     age sex    race            new_county  
    ##    <fct>            <dbl> <fct>  <fct>           <fct>       
    ##  1 In hospital         30 Male   Hispanic, Black litchfield  
    ##  2 In hospital         23 Female Hispanic, Black windham     
    ##  3 Outside hospital    50 Female Hispanic, Black litchfield  
    ##  4 Outside hospital    29 Female Black           middlesex   
    ##  5 Outside hospital    54 Female Hispanic, Black new london  
    ##  6 Outside hospital    32 Female Hispanic, Black out of state
    ##  7 In hospital         39 Male   Hispanic, Black middlesex   
    ##  8 Outside hospital    34 Female Hispanic, Black litchfield  
    ##  9 Outside hospital    27 Male   Hispanic, Black litchfield  
    ## 10 In hospital         25 Female Hispanic, Black middlesex   
    ## # … with 3,470 more rows

``` r
# create model
model=
  glm(death_location ~ age + sex + race + new_county, data = drug_death_bin, family = "binomial") 


model %>%
  broom::glance()%>% 
  knitr::kable(digits = 3)
```

|  null.deviance|  df.null|     logLik|       AIC|       BIC|  deviance|  df.residual|
|--------------:|--------:|----------:|---------:|---------:|---------:|------------:|
|       4275.072|     3479|  -2096.793|  4227.586|  4332.217|  4193.586|         3463|

``` r
model %>%
  broom::tidy()%>% 
  knitr::kable(digits = 3)
```

| term                    |  estimate|  std.error|  statistic|  p.value|
|:------------------------|---------:|----------:|----------:|--------:|
| (Intercept)             |    -0.057|      0.413|     -0.137|    0.891|
| age                     |     0.006|      0.003|      1.968|    0.049|
| sexFemale               |     0.188|      0.085|      2.202|    0.028|
| sexUnknown              |    10.405|    196.968|      0.053|    0.958|
| raceBlack               |    -0.237|      0.408|     -0.580|    0.562|
| raceHispanic, White     |     0.115|      0.637|      0.180|    0.857|
| raceAsian               |    -0.181|      0.405|     -0.448|    0.654|
| raceOther               |    -0.045|      0.833|     -0.054|    0.957|
| raceHispanic, Black     |     0.419|      0.393|      1.067|    0.286|
| new\_countyhartford     |     0.404|      0.115|      3.495|    0.000|
| new\_countytolland      |     0.081|      0.178|      0.452|    0.651|
| new\_countynew london   |     0.571|      0.211|      2.710|    0.007|
| new\_countymiddlesex    |     0.092|      0.115|      0.796|    0.426|
| new\_countynew haven    |     0.186|      0.154|      1.211|    0.226|
| new\_countywindham      |    -0.073|      0.220|     -0.330|    0.741|
| new\_countyfairfield    |     0.438|      0.237|      1.849|    0.064|
| new\_countyout of state |     0.554|      0.229|      2.426|    0.015|

``` r
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = death_location ~ age + sex + race + new_county, 
    ##     family = "binomial", data = drug_death_bin)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8360  -1.3649   0.7588   0.8602   1.2600  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -0.056709   0.412829  -0.137 0.890740    
    ## age                      0.005997   0.003047   1.968 0.049084 *  
    ## sexFemale                0.188137   0.085440   2.202 0.027667 *  
    ## sexUnknown              10.404661 196.967717   0.053 0.957872    
    ## raceBlack               -0.236771   0.408280  -0.580 0.561966    
    ## raceHispanic, White      0.114777   0.637192   0.180 0.857051    
    ## raceAsian               -0.181164   0.404719  -0.448 0.654420    
    ## raceOther               -0.045259   0.833020  -0.054 0.956671    
    ## raceHispanic, Black      0.418701   0.392529   1.067 0.286119    
    ## new_countyhartford       0.403603   0.115496   3.495 0.000475 ***
    ## new_countytolland        0.080668   0.178393   0.452 0.651129    
    ## new_countynew london     0.570579   0.210525   2.710 0.006723 ** 
    ## new_countymiddlesex      0.091581   0.115124   0.796 0.426322    
    ## new_countynew haven      0.186435   0.154007   1.211 0.226062    
    ## new_countywindham       -0.072534   0.219796  -0.330 0.741397    
    ## new_countyfairfield      0.437746   0.236777   1.849 0.064491 .  
    ## new_countyout of state   0.554449   0.228581   2.426 0.015283 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4275.1  on 3479  degrees of freedom
    ## Residual deviance: 4193.6  on 3463  degrees of freedom
    ## AIC: 4227.6
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
# significant p = age, sexFemale, counties: Hartford, New London, Out of State

model %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```

| term                    |  log\_OR|         OR|  p.value|
|:------------------------|--------:|----------:|--------:|
| (Intercept)             |   -0.057|      0.945|    0.891|
| age                     |    0.006|      1.006|    0.049|
| sexFemale               |    0.188|      1.207|    0.028|
| sexUnknown              |   10.405|  33013.149|    0.958|
| raceBlack               |   -0.237|      0.789|    0.562|
| raceHispanic, White     |    0.115|      1.122|    0.857|
| raceAsian               |   -0.181|      0.834|    0.654|
| raceOther               |   -0.045|      0.956|    0.957|
| raceHispanic, Black     |    0.419|      1.520|    0.286|
| new\_countyhartford     |    0.404|      1.497|    0.000|
| new\_countytolland      |    0.081|      1.084|    0.651|
| new\_countynew london   |    0.571|      1.769|    0.007|
| new\_countymiddlesex    |    0.092|      1.096|    0.426|
| new\_countynew haven    |    0.186|      1.205|    0.226|
| new\_countywindham      |   -0.073|      0.930|    0.741|
| new\_countyfairfield    |    0.438|      1.549|    0.064|
| new\_countyout of state |    0.554|      1.741|    0.015|

Analysis
========

In order to understand the likelihood of a drug related death occuring in the hospital versus not in the hospital, we binarized the variable `death_location` as the outcome (1 = In the hospital, 0 = Not in the hospital). For this logistic regression analysis we used a generlized logistic model with the predictors age, sex, race and county of residence. `Age` was treated as a continuous variable. `Sex` was categorical (Male, Female, and Unknown) with Male as the reference category based on frequency. `Race` was categorical with White, Black, Hispanic White, Asian, Other, Hispanic Black; White was the reference category based on frequency and NAs were dropped pre-analysis (n= 16). Finally the 8 `Counties` of CT were converted to factor variables with all other counties being converted to "out of state" with Litchfield Country as the reference variable based on frequency, NAs were dropped (n= 7).

The significant variables in the model (p-value &lt; 0.05) were age, Female, Hartford and New London Counties and the "Out of State" category. TERM OR P-VALUE Odds of dying in the hospital vs. outside the hospital age 1.006 0.049 (for every 1-year increase in age) female 1.207 0.028 (vs. Male) Hartford 1.497 0.000 (vs. Litchfield) New London 1.769 0.007 " Out of State 1.741 0.015 "
