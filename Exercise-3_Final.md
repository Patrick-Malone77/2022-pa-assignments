
# Exercise 3

## Setting up data and environment

## We first need to do a few things before we can manipulate the data.

``` r
# set path for R to find our data
data_path <- "C:/Users/patma/OneDrive/Desktop/USPTO_data/"
```

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.6     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

## 1. Load data

We’ll load application data only here (you are welcome to load the other
three files as well). Because we are loading from a .parquet format
file, we’ll use library `arrow` and the functions `read_parquet()`. For
the rest of the files, we can use function `read_csv()` which comes with
a package `readr` (which is included in `tidyverse` set of packages, so
if we are loading `tidyverse` there is no need to also load `readr`).
Note that the path to the data file on my computer is defined above, in
the `data_path` variable.

``` r
library(arrow) # to be able to load data in the .parquet format
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
# read application data
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
```

To inspect the top slice of the data, we can simply call it:

``` r
app_data_sample
```

    ## # A tibble: 2,018,477 × 16
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # … with 2,018,467 more rows, and 12 more variables:
    ## #   examiner_name_middle <chr>, examiner_id <dbl>, examiner_art_unit <dbl>,
    ## #   uspc_class <chr>, uspc_subclass <chr>, patent_number <chr>,
    ## #   patent_issue_date <date>, abandon_date <date>, disposal_type <chr>,
    ## #   appl_status_code <dbl>, appl_status_date <chr>, tc <dbl>

### Get gender for examiners

We’ll get gender based on the first name of the examiner, which is
recorded in the field `examiner_name_first`. We’ll use library `gender`
for that, relying on a modified version of their own
[example](https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html).

Note that there are over 2 million records in the applications table –
that’s because there are many records for each examiner, as many as the
number of applications that examiner worked on during this time frame.
Our first step therefore is to get all *unique* names in a separate list
`examiner_names`. We will then guess gender for each one and will join
this table back to the original dataset. So, let’s get names without
repetition:

``` r
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it
# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)
examiner_names
```

    ## # A tibble: 2,595 × 1
    ##    examiner_name_first
    ##    <chr>              
    ##  1 JACQUELINE         
    ##  2 BEKIR              
    ##  3 CYNTHIA            
    ##  4 MARY               
    ##  5 MICHAEL            
    ##  6 LINDA              
    ##  7 KARA               
    ##  8 VANESSA            
    ##  9 TERESA             
    ## 10 SUN                
    ## # … with 2,585 more rows

Now let’s use function `gender()` as shown in the example for the
package to attach a gender and probability to each name and put the
results into the table `examiner_names_gender`

``` r
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
examiner_names_gender
```

    ## # A tibble: 1,822 × 3
    ##    examiner_name_first gender proportion_female
    ##    <chr>               <chr>              <dbl>
    ##  1 AARON               male              0.0082
    ##  2 ABDEL               male              0     
    ##  3 ABDOU               male              0     
    ##  4 ABDUL               male              0     
    ##  5 ABDULHAKIM          male              0     
    ##  6 ABDULLAH            male              0     
    ##  7 ABDULLAHI           male              0     
    ##  8 ABIGAIL             female            0.998 
    ##  9 ABIMBOLA            female            0.944 
    ## 10 ABRAHAM             male              0.0031
    ## # … with 1,812 more rows

Finally, let’s join that table back to our original applications data
and discard the temporary tables we have just created to reduce clutter
in our environment.

``` r
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)
# joining gender back to the dataset
app_data_sample <- app_data_sample %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")
# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4543747 242.7    7547112 403.1  4942566 264.0
    ## Vcells 49526317 377.9   92424448 705.2 79842104 609.2

### Guess the examiner’s race

We’ll now use package `wru` to estimate likely race of an examiner. Just
like with gender, we’ll get a list of unique names first, only now we
are using surnames.

``` r
library(wru)
examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_surnames
```

    ## # A tibble: 3,806 × 1
    ##    surname   
    ##    <chr>     
    ##  1 HOWARD    
    ##  2 YILDIRIM  
    ##  3 HAMILTON  
    ##  4 MOSHER    
    ##  5 BARR      
    ##  6 GRAY      
    ##  7 MCMILLIAN 
    ##  8 FORD      
    ##  9 STRZELECKA
    ## 10 KIM       
    ## # … with 3,796 more rows

We’ll follow the instructions for the package outlined here
<https://github.com/kosukeimai/wru>.

``` r
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

``` r
examiner_race
```

    ## # A tibble: 3,806 × 6
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198
    ## # … with 3,796 more rows

As you can see, we get probabilities across five broad US Census
categories: white, black, Hispanic, Asian and other. (Some of you may
correctly point out that Hispanic is not a race category in the US
Census, but these are the limitations of this package.)

Our final step here is to pick the race category that has the highest
probability for each last name and then join the table back to the main
applications table. See this example for comparing values across
columns: <https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/>.
And this one for `case_when()` function:
<https://dplyr.tidyverse.org/reference/case_when.html>.

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))
examiner_race
```

    ## # A tibble: 3,806 × 8
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth max_race_p race 
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <dbl> <chr>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333      0.643 white
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372      0.861 white
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309      0.702 white
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185      0.947 white
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271      0.827 white
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324      0.687 white
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463      0.574 black
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313      0.620 white
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318      0.666 white
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198      0.945 Asian
    ## # … with 3,796 more rows

Let’s join the data back to the applications table.

``` r
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)
app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4958478 264.9    7547112 403.1  7547112 403.1
    ## Vcells 53325960 406.9   92424448 705.2 90530443 690.7

### Examiner’s tenure

To figure out the timespan for which we observe each examiner in the
applications data, let’s find the first and the last observed date for
each examiner. We’ll first get examiner IDs and application dates in a
separate table, for ease of manipulation. We’ll keep examiner ID (the
field `examiner_id`), and earliest and latest dates for each application
(`filing_date` and `appl_status_date` respectively). We’ll use functions
in package `lubridate` to work with date and time values.

``` r
library(lubridate) # to work with dates
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates
```

    ## # A tibble: 2,018,477 × 3
    ##    examiner_id filing_date appl_status_date  
    ##          <dbl> <date>      <chr>             
    ##  1       96082 2000-01-26  30jan2003 00:00:00
    ##  2       87678 2000-10-11  27sep2010 00:00:00
    ##  3       63213 2000-05-17  30mar2009 00:00:00
    ##  4       73788 2001-07-20  07sep2009 00:00:00
    ##  5       77294 2000-04-10  19apr2001 00:00:00
    ##  6       68606 2000-04-28  16jul2001 00:00:00
    ##  7       89557 2004-01-26  15may2017 00:00:00
    ##  8       97543 2000-06-23  03apr2002 00:00:00
    ##  9       98714 2000-02-04  27nov2002 00:00:00
    ## 10       65530 2002-02-20  23mar2009 00:00:00
    ## # … with 2,018,467 more rows

The dates look inconsistent in terms of formatting. Let’s make them
consistent. We’ll create new variables `start_date` and `end_date`.

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018)
```

Let’s now identify the earliest and the latest date for each examiner
and calculate the difference in days, which is their tenure in the
organization.

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )
examiner_dates
```

    ## # A tibble: 5,649 × 4
    ##    examiner_id earliest_date latest_date tenure_days
    ##          <dbl> <date>        <date>            <dbl>
    ##  1       59012 2004-07-28    2015-07-24         4013
    ##  2       59025 2009-10-26    2017-05-18         2761
    ##  3       59030 2005-12-12    2017-05-22         4179
    ##  4       59040 2007-09-11    2017-05-23         3542
    ##  5       59052 2001-08-21    2007-02-28         2017
    ##  6       59054 2000-11-10    2016-12-23         5887
    ##  7       59055 2004-11-02    2007-12-26         1149
    ##  8       59056 2000-03-24    2017-05-22         6268
    ##  9       59074 2000-01-31    2017-03-17         6255
    ## 10       59081 2011-04-21    2017-05-19         2220
    ## # … with 5,639 more rows

Joining back to the applications data.

``` r
app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  4972772 265.6   13531868  722.7  13531868  722.7
    ## Vcells 65705055 501.3  133267204 1016.8 133266269 1016.8

## 3. Descriptive statistics

Let’s look at distribution of gender, race and tenure, overall in the
organization, and by technology centers (TCs) and workgroups.

### Overall distributions of gender, race and tenure.

We can we can start with simple frequencies.

``` r
subset_app_data <- app_data_sample %>% 
  mutate(race = as_factor(race), gender = as_factor(gender)) %>% 
  select(gender, race, tenure_days) 
subset_app_data %>% 
  count(gender) %>% 
  mutate(pct = n/sum(n))
```

    ## # A tibble: 3 × 3
    ##   gender       n   pct
    ##   <fct>    <int> <dbl>
    ## 1 female  571227 0.283
    ## 2 male   1143391 0.566
    ## 3 <NA>    303859 0.151

``` r
subset_app_data %>% 
  count(race) %>% 
  mutate(pct = n/sum(n))
```

    ## # A tibble: 5 × 3
    ##   race           n      pct
    ##   <fct>      <int>    <dbl>
    ## 1 white    1279353 0.634   
    ## 2 black      90027 0.0446  
    ## 3 Asian     588988 0.292   
    ## 4 Hispanic   59657 0.0296  
    ## 5 other        452 0.000224

We can also use library `skimr` to skim the data quickly.

``` r
library(skimr)
subset_app_data %>%  
  skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 2018477    |
| Number of columns                                | 3          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 2          |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts                                        |
|:--------------|----------:|--------------:|:--------|---------:|:--------------------------------------------------|
| gender        |    303859 |          0.85 | FALSE   |        2 | mal: 1143391, fem: 571227                         |
| race          |         0 |          1.00 | FALSE   |        5 | whi: 1279353, Asi: 588988, bla: 90027, His: 59657 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |      sd |  p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|--------:|--------:|----:|-----:|-----:|-----:|-----:|:------|
| tenure_days   |         0 |             1 | 5531.37 | 1100.91 |  27 | 4962 | 6090 | 6336 | 6518 | ▁▁▁▂▇ |

## Creating Art Unit size variable and joining it to main table

``` r
art_unit_size = app_data_sample %>% group_by (examiner_art_unit) %>% count() %>% select(examiner_art_unit,art_unit_size = n)
app_data_sample = app_data_sample %>% left_join(art_unit_size,by = "examiner_art_unit")
```

## Creating Gender ratio variable in every art unit and joining it to the main table

``` r
art_unit_gender_size = app_data_sample %>% group_by(examiner_art_unit,gender) %>%
  count() %>%  filter(!is.na(gender)) %>% filter(gender == 'female')

Table_gender_size <- art_unit_gender_size %>% 
  left_join(art_unit_size, by = "examiner_art_unit")

Table_gender_size$female_to_male_ratio = Table_gender_size$n/(Table_gender_size$art_unit_size - Table_gender_size$n)

df = subset(Table_gender_size, select = c(female_to_male_ratio, examiner_art_unit))

app_data_sample = app_data_sample %>% left_join(df, by = "examiner_art_unit")
```

## 4. Linear Models

## Libraries for the regression

``` r
library(modelsummary)
library(kableExtra)
```

    ## Warning in !is.null(rmarkdown::metadata$output) && rmarkdown::metadata$output
    ## %in% : 'length(x) = 2 > 1' in coercion to 'logical(1)'

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(gt)
```

    ## 
    ## Attaching package: 'gt'

    ## The following object is masked from 'package:modelsummary':
    ## 
    ##     escape_latex

## OLS 1 - Using all variabes to explain Tenure_days

``` r
OLS_1 = lm(tenure_days ~ art_unit_size + as.factor(gender) + as.factor(race) + female_to_male_ratio  , data=app_data_sample)
summary(OLS_1)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + as.factor(gender) + 
    ##     as.factor(race) + female_to_male_ratio, data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5515.5  -549.7   445.3   750.2  1420.9 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)              5.067e+03  2.739e+00 1850.043   <2e-16 ***
    ## art_unit_size            3.985e-02  1.559e-04  255.651   <2e-16 ***
    ## as.factor(gender)male   -2.630e+01  1.821e+00  -14.437   <2e-16 ***
    ## as.factor(race)black     1.521e+02  4.716e+00   32.243   <2e-16 ***
    ## as.factor(race)Hispanic -3.067e+02  4.894e+00  -62.670   <2e-16 ***
    ## as.factor(race)other    -5.762e+02  5.011e+01  -11.499   <2e-16 ***
    ## as.factor(race)white    -4.036e+00  1.948e+00   -2.072   0.0382 *  
    ## female_to_male_ratio     2.109e+02  2.194e+00   96.144   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1065 on 1711234 degrees of freedom
    ##   (307235 observations deleted due to missingness)
    ## Multiple R-squared:  0.05273,    Adjusted R-squared:  0.05273 
    ## F-statistic: 1.361e+04 on 7 and 1711234 DF,  p-value: < 2.2e-16

## OLS 2 - Using gender and art_unit_size to explain Tenure_days

``` r
OLS_2 = lm(tenure_days ~ art_unit_size + as.factor(gender) , data=app_data_sample)
summary(OLS_2)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + as.factor(gender), 
    ##     data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5525.7  -552.8   445.3   766.1  1237.6 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            5.175e+03  2.161e+00 2394.58   <2e-16 ***
    ## art_unit_size          4.209e-02  1.543e-04  272.75   <2e-16 ***
    ## as.factor(gender)male -8.279e+01  1.741e+00  -47.54   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1070 on 1714615 degrees of freedom
    ##   (303859 observations deleted due to missingness)
    ## Multiple R-squared:  0.04454,    Adjusted R-squared:  0.04454 
    ## F-statistic: 3.997e+04 on 2 and 1714615 DF,  p-value: < 2.2e-16

## OLS 3 - Using Gender Ratio and art_unit_size to explain tenure days

``` r
OLS_3 = lm(tenure_days ~ art_unit_size + female_to_male_ratio  , data=app_data_sample)
summary(OLS_3)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_unit_size + female_to_male_ratio, 
    ##     data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5681.5  -533.9   443.6   759.8  1290.7 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          5.017e+03  1.706e+00  2941.3   <2e-16 ***
    ## art_unit_size        4.090e-02  1.414e-04   289.2   <2e-16 ***
    ## female_to_male_ratio 2.462e+02  1.983e+00   124.2   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1070 on 2014154 degrees of freedom
    ##   (4320 observations deleted due to missingness)
    ## Multiple R-squared:  0.05395,    Adjusted R-squared:  0.05395 
    ## F-statistic: 5.743e+04 on 2 and 2014154 DF,  p-value: < 2.2e-16

## OLS 4 - Using race, gender ratio & art unit size to explain Tenure_days

``` r
OLS_4 <- lm(tenure_days ~ female_to_male_ratio + as.factor(race) + art_unit_size  , data=app_data_sample)
summary(OLS_4)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ female_to_male_ratio + as.factor(race) + 
    ##     art_unit_size, data = app_data_sample)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5682.1  -531.5   438.0   757.9  1430.6 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)              5.019e+03  1.974e+00 2542.282  < 2e-16 ***
    ## female_to_male_ratio     2.430e+02  1.991e+00  122.024  < 2e-16 ***
    ## as.factor(race)black     1.047e+02  3.838e+00   27.282  < 2e-16 ***
    ## as.factor(race)Hispanic -3.016e+02  4.593e+00  -65.654  < 2e-16 ***
    ## as.factor(race)other    -5.695e+02  5.029e+01  -11.323  < 2e-16 ***
    ## as.factor(race)white     1.231e+01  1.701e+00    7.237 4.58e-13 ***
    ## art_unit_size            4.047e-02  1.418e-04  285.414  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1069 on 2014150 degrees of freedom
    ##   (4320 observations deleted due to missingness)
    ## Multiple R-squared:  0.0567, Adjusted R-squared:  0.0567 
    ## F-statistic: 2.018e+04 on 6 and 2014150 DF,  p-value: < 2.2e-16

## Results

The criteria that I used to analyze the four OLS regressions are the
following: Adjusted r-squared, P-value, residual standard error, and the
number of observations deleted from the data for the regression.

### Adjusted r-squared

When it comes to the adjusted r-squared, the regression with the highest
value is the one that should be favoured, all things equal. That said,
the regression with the highest adjusted r-squared value is the 4th
regression with a value of 0.0567. This tells me that this specific
regression is improved with the variables used (female to male ratio,
race, and art unit size). The next closest adjusted r-squared, 0.05395,
comes from the third regression which includes the variables art unit
size and female to male ratio. Overall, it would seem that the addition
of the race variable positively impacts the OLS model in comparison to
model 3.

### P-value analysis

A low p-value shows that the results are replicable. Considering all 4
regression models have the same p-value of 2.2e-16, this statistic
demonstrates that all regressions are equally acceptable to use and
provide the same probability that the tenure days prediction can be
accurately predicted using the models.

\###Residual standard error

The smaller the residual standard error, the better the model fits the
data set. By looking at the 4 values, they are all relatively close to
one another, however, OLS 3 provides the lowest RSE of 0.053% (1070 on
2014154 DFs) and OLS 4 with the second lowest RSE of 0.0544% (1069 on
2014150 DFs). While OLS 3 & 4 are the better fits for the data, all four
regression models demonstrate very low RSE suggesting that all models
generally fit the data accurately.

### Number of observations deleted

OLS 1 & 2 have over 300K records deleted from the regression and it is
as a result of including the gender variable into the model. There are
many instances in which the data does not have a gender listed for an
individual, thus forcing the model to filter out the N/A inputs and
reducing the data provided. On the other hand, OLS 3 & 4 have only 4320
records deleted due to missingness, thus those regressions are providing
a better representation of the data given. The reason there are fewer
observations deleted is as a result of the female-to-male ratio variable
rather than the gender variable on its own. The female-to-ratio variable
is able to filter out the N/A inputs and provides the ratio to that
individuals unit instead.

### Conclusion

Overall, it seems that OLS 4 is the best regression for the data. It
provides the highest adjusted r-square value, has the fewest deleted
observations, the second lowest RSE and the same p-value as the other
regression models. This model does not include all variables as it
excludes gender, but it provides the best outcome. This tells me that
the gender variable is better off being blended within the
female-to-male ratio rather than being a variable on its own as the
adjusted r-square is higher with only the ratio.

## Improvements

The main improvement I would suggest for the regression is to run a
regression using 75% of the data as training data and 25% of the data as
testing data. This will allow us to better see if our models are
accurately predicting the tenure days of the workers and which model is
providing the best prediction. I would believe that since we are using
the entire data set to generate our regression models, it is more likely
to achieve a lower p-value, for example, thus introducing biases in the
data. Another improvement, if the scope is to analyze smaller TCs or
WGs, would be to create individual regression models for each tc or wg
and analyze if there are different patterns across the groups compared
to as a whole.

## Contributors to R code:

### Ranvir, Hima, Patrick, Adel, Shan
