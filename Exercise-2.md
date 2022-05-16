Exercise 2
================

## Contributors

#### Hima Aryasomayajula

#### Patrick Malone

#### Ranvir Kumar

#### Shan Jiang

## Setting up data and environment

We first need to do a few things before we can manipulate the data.

``` r
# set path for R to find our data
data_path <- "C:/Users/patma/OneDrive/Desktop/People Analytics/USPTO_data/"
data_path <- "C:/Users/Hima25/OneDrive/Documents/USPTO_data/"
setwd(data_path)

# load the necessary packages ("extensions")
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

``` r
library(gender)
library(wru)
library(predictrace)
```

    ## 
    ## Attaching package: 'predictrace'

    ## The following object is masked from 'package:wru':
    ## 
    ##     predict_race

``` r
library(lubridate) # to work with dates
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(skimr) # for summaries of data
library(arrow) # to be able to load data in the .parquet format
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     duration

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
library(lattice)
library(polycor)
```

## Questions

## 1. Load the 4 data files as outlined in the project description

``` r
# read application data
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
```

    examiner_aus <- read_csv("examiner_aus.csv")
    examiner_gs <- read_csv("examiner_gs.csv")
    examiner_ids <- read_csv("examiner_ids.csv")

## 2. Using `app_data_sample` data, create individual-level variables for

#### • gender (based on name, use package `gender`)

#### • race (based on name, use package `wru`)

#### • tenure (from first observed to last observed; use package `lubridate`)

#### Find examiner GENDER based on firstname

###### install_genderdata_package() \# only run the first time

``` r
examiner_gender <- app_data_sample %>% 
  distinct(examiner_name_first) %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender
  )
  examiner_gender
```

    ## # A tibble: 1,822 × 2
    ##    examiner_name_first gender
    ##    <chr>               <chr> 
    ##  1 AARON               male  
    ##  2 ABDEL               male  
    ##  3 ABDOU               male  
    ##  4 ABDUL               male  
    ##  5 ABDULHAKIM          male  
    ##  6 ABDULLAH            male  
    ##  7 ABDULLAHI           male  
    ##  8 ABIGAIL             female
    ##  9 ABIMBOLA            female
    ## 10 ABRAHAM             male  
    ## # … with 1,812 more rows

``` r
## Joining gender back to the dataset - app_data_sample table
Table_All <- app_data_sample %>% 
  left_join(examiner_gender, by = "examiner_name_first")


## Check for null values for gender in the new table
sum(is.na(Table_All$gender))
```

    ## [1] 303859

``` r
## Check for null values for firstname in the original table
sum(is.na(app_data_sample$examiner_name_first))
```

    ## [1] 0

##### ISSUES FOUND

-   issue 1: Gender column is calculated correctly in the new table but
    after merging into the original table, it shows up as NA for 300K
    examiners; possible issue with merging.

#### Find examiner RACE based on lastname

``` r
# calculating race using the wru package's predict_race function using lastname of examiner
LN_Race <- Table_All %>% 
  mutate(Table_All, race<- predict_race(c(Table_All$examiner_name_last)))

# remove unused columns
Table_All_2 = subset(LN_Race, select = -c(name,match_name,probability_american_indian,probability_asian, probability_black,probability_hispanic,probability_white, probability_2races))

## Check for null values for lastname in the original table
sum(is.na(app_data_sample$examiner_name_last))
```

    ## [1] 0

``` r
## Check for null values for likely_race in the new table
sum(is.na(Table_All_2$likely_race))
```

    ## [1] 314346

##### ISSUES FOUND

-   issue 2: 314K of race fields were not calculated and were NA

#### Find TENURE based on filing_date and appl_status_date

###### Let’s find the first and the last observed date for each examiner

We’ll first get examiner IDs and application dates in a separate
dataframe (table), for ease of manipulation. We’ll keep examiner ID (the
field `examiner_id`), and earliest and latest dates for each application
(`filing_date` and `appl_status_date` respectively).

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
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))
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
    tenure = interval(earliest_date, latest_date) %/% days(1)
    )

examiner_dates
```

    ## # A tibble: 5,649 × 4
    ##    examiner_id earliest_date latest_date tenure
    ##          <dbl> <date>        <date>       <dbl>
    ##  1       59012 2004-07-28    2015-07-24    4013
    ##  2       59025 2009-10-26    2017-05-18    2761
    ##  3       59030 2005-12-12    2017-05-22    4179
    ##  4       59040 2007-09-11    2017-05-23    3542
    ##  5       59052 2001-08-21    2007-02-28    2017
    ##  6       59054 2000-11-10    2016-12-23    5887
    ##  7       59055 2004-11-02    2007-12-26    1149
    ##  8       59056 2000-03-24    2017-05-22    6268
    ##  9       59074 2000-01-31    2017-03-17    6255
    ## 10       59081 2011-04-21    2017-05-19    2220
    ## # … with 5,639 more rows

``` r
skim(examiner_dates)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | examiner_dates |
| Number of rows                                   | 5649           |
| Number of columns                                | 4              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| Date                                             | 2              |
| numeric                                          | 2              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| earliest_date |         0 |             1 | 2000-01-02 | 2016-03-03 | 2003-01-21 |     2325 |
| latest_date   |         0 |             1 | 2000-09-14 | 9468-10-16 | 2017-05-19 |      888 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |     mean |       sd |    p0 |      p25 |   p50 |      p75 |    p100 | hist  |
|:--------------|----------:|--------------:|---------:|---------:|------:|---------:|------:|---------:|--------:|:------|
| examiner_id   |         1 |             1 | 78752.86 | 13575.30 | 59012 | 66531.75 | 75346 | 93750.75 |   99990 | ▇▆▃▂▇ |
| tenure        |         0 |             1 |  5844.53 | 54449.66 |    27 |  3125.00 |  4918 |  6097.00 | 2727903 | ▇▁▁▁▁ |

##### Merge tenure column to main table

``` r
Table_All_3 <- merge(x=Table_All_2,y=examiner_dates,by="examiner_id",all.x=TRUE)
```

## 3. Using tables and plots (e.g., histograms or other density plots), describe

#### • Overall distribution of gender, race and tenure

#### • Distributions over TCs (Technology Centers <https://www.uspto.gov/patents/contact-patents/patent-> technology-centers-management )

#### • Distributions over WGs (Workgroups <https://www.uspto.gov/patents/contact-patents/tc-1600-> management-roster )

#### Overall Distribution - gender,race,tenure

##### ggplot for gender

``` r
plot_gender = ggplot(examiner_gender, aes(gender)) + geom_bar()
plot_gender
```

![](Exercise-2_Working-File_with_group_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

##### ggplot for race

``` r
plot_race = ggplot(LN_Race, aes(likely_race)) + geom_bar()
plot_race
```

![](Exercise-2_Working-File_with_group_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### Distributions over TCs

``` r
## Group by TC - Gender

plot_gender_by_TC = ggplot(Table_All_3, aes(x=tc, fill=gender)) +
    geom_histogram(binwidth = 50)

plot_gender_by_TC
```

![](Exercise-2_Working-File_with_group_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
## Group by TC - Race

plot_race_by_TC = ggplot(Table_All_3, aes(x=tc, fill=likely_race)) +
    geom_histogram(binwidth = 50)

plot_race_by_TC
```

![](Exercise-2_Working-File_with_group_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
## Group by TC - Average Tenure 
temp_avg_tenure <- Table_All_3 %>% group_by(tc) %>%
   summarise_at(vars(tenure), list(avg_tenure = mean)) %>%
  filter(tc %in% c(1600,1700,2100,2400))



plot_tenure_by_TC = xyplot(tc ~ avg_tenure, temp_avg_tenure)

plot_tenure_by_TC
```

![](Exercise-2_Working-File_with_group_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

#### Distributions over WGs

###### Create WG Column

``` r
Table_All_4 <- Table_All_3 %>% group_by(tc) %>%
  mutate( wg = examiner_art_unit%%100)

## Group by wg - Gender 

plot_gender_by_WG = ggplot(Table_All_4, aes(x=wg, fill=gender)) +
    geom_histogram(binwidth = .5)

plot_gender_by_WG
```

![](Exercise-2_Working-File_with_group_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
## Group by wg - Race

plot_race_by_WG = ggplot(Table_All_4, aes(x=wg, fill=likely_race)) +
    geom_histogram(binwidth = .5)

plot_race_by_WG
```

![](Exercise-2_Working-File_with_group_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
## Group by wg - Average Tenure 
temp_avg_tenure_wg <- Table_All_4 %>% group_by(tc,wg) %>%
   summarise_at(vars(tenure), list(avg_tenure = mean)) 

plot_tenure_by_wg = ggplot(temp_avg_tenure_wg, aes(x = wg, y = avg_tenure, colour = tc)) +
  geom_point()+ facet_wrap( ~ tc)


plot_tenure_by_wg
```

![](Exercise-2_Working-File_with_group_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

## 4. Correlate gender and race with tenure

### • Overall

##### Heterogeneous Correlation Matrix

``` r
 data <- data.frame(Table_All_4$gender, Table_All_4$likely_race, Table_All_4$tenure)
 hetcor(data)
```

    ## data contain one or more character variables
    ## the values of which are ordered alphabetically

    ## 
    ## Two-Step Estimates
    ## 
    ## Correlations/Type of Correlation:
    ##                         Table_All_4.gender Table_All_4.likely_race
    ## Table_All_4.gender                       1              Polychoric
    ## Table_All_4.likely_race           0.004841                       1
    ## Table_All_4.tenure                 0.02638                -0.01305
    ##                         Table_All_4.tenure
    ## Table_All_4.gender              Polyserial
    ## Table_All_4.likely_race         Polyserial
    ## Table_All_4.tenure                       1
    ## 
    ## Standard Errors:
    ##                         Table_All_4.gender Table_All_4.likely_race
    ## Table_All_4.gender                                                
    ## Table_All_4.likely_race           0.001352                        
    ## Table_All_4.tenure                0.001504               0.0008329
    ## 
    ## n = 1457146 
    ## 
    ## P-values for Tests of Bivariate Normality:
    ##                         Table_All_4.gender Table_All_4.likely_race
    ## Table_All_4.gender                                                
    ## Table_All_4.likely_race                  0                        
    ## Table_All_4.tenure                       0                       0

### • By TC

##### Heterogeneous Correlation Matrix

``` r
 data2 <- data.frame(Table_All_4$gender, Table_All_4$likely_race, Table_All_4$tenure) %>%        group_by(Table_All_4$tc)
 #hetcor(data2)
```

#### ISSUES FOUND

-issue 3 - Error in FUN(X\[\[i\]\], …) : columns must be numeric,
factors, logical, or character.

#### Cleaning up

``` r
rm(examiner_gender)
rm(examiner_dates)
rm(LN_Race)
rm(Table_All)
rm(Table_All_2)
rm(Table_All_3)
rm(Table_All_4)
rm(temp_avg_tenure)
rm(temp_avg_tenure_wg)
rm(data)
rm(data2)
gc()
```

    ##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells   5032210  268.8   16672928  890.5  16672928  890.5
    ## Vcells 151859380 1158.6  343666304 2622.0 343639862 2621.8

## 5. In a few sentences, what are your conclusions about gender and race distribution in this organization?

Based on our observations, the overall gender difference between the
number of Male and Female at USPO is around 250. Initial impressions,
not taking into account various “NA” fields due to the function’s
inability to identify gender for all, seems to be pointing in the
direction that overall, the difference between the number of male and
female workers in this organization is negligible. A difference of 250,
considering the overall size of the organization, is not very
significant to make any concrete statements or assumptions. It is
important to note however, that the difference is more pronounced when
the gender distribution is looked at for different Technology centers
(TCs). Specially in TC 1700 (Chemical and Materials Engineering), 2100
(Computer architecture and software) and 2400 (Networking, Multiplexing,
Cable, and Security, the magnitude of difference between Male and Female
is quite pronounced and it can be said that these TCs are male
dominated. TC 1600 (Biotechnology & Organic Chemistry) is fairly equally
distributed in terms of gender.

Based on our observations, the overall race distribution across USPO
shows that the organization is primarily dominated by individuals of
Asian and White ethnicity.TC 2100 and 2400 has almost the same number of
White and Asian individuals where as TC 1600 and 1700 is primarily
dominated by White ethnicity, with Asians in close second position.

Distribution of gender in different work groups show similar result as
above with Males dominating most of the WGs, while distribution of race
in different work groups again show similar results as above with Whites
and Asians dominating all the WGs and White leading the pack for most of
the WGs.

## What are your thoughts on the correlations above?

The heterogeneous correlation types observed include polyserial and
polychoric. Polyserial correlation exists between numeric and
categorical variables, tenure vs. gender (with correlation coefficient
of 0.026), and tenure vs. race (with correlation coefficient of -0.013)
in this case. Polychoric correlation exists between two categorical
variables, which is gender vs. race with correlation relationship as
0.0048.

The rule of thumb is that when the absolute value of the correlation
coefficient is above 0.8 we say there is a problematic relationship
between the two variables. However, we didn’t observe that in this case.

So we can conclude that the three variables are not highly correlated,
each has its own specific contribution to estimation and shall all be
considered in the following model.
