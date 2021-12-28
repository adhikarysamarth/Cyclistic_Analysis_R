Cyclist Bike Share Analysis - Samarth
================

# Step 1: (Collect Data)

## Load Packages

``` r
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(readxl)
```

## Load Individual Dataframes

``` r
oct_2020 <- read_excel("Trip Data/Analysis Files/Excel Files/2020_10.xlsx")
nov_2020 <- read_excel("Trip Data/Analysis Files/Excel Files/2020_11.xlsx")
dec_2020 <- read_excel("Trip Data/Analysis Files/Excel Files/2020_12.xlsx")
jan_2021 <- read_excel("Trip Data/Analysis Files/Excel Files/2021_01.xlsx")
feb_2021 <- read_excel("Trip Data/Analysis Files/Excel Files/2021_02.xlsx")
mar_2021 <- read_excel("Trip Data/Analysis Files/Excel Files/2021_03.xlsx")
apr_2021 <- read_excel("Trip Data/Analysis Files/Excel Files/2021_04.xlsx")
may_2021 <- read_excel("Trip Data/Analysis Files/Excel Files/2021_05.xlsx")
jun_2021 <- read_excel("Trip Data/Analysis Files/Excel Files/2021_06.xlsx")
jul_2021 <- read_excel("Trip Data/Analysis Files/Excel Files/2021_07.xlsx")
aug_2021 <- read_excel("Trip Data/Analysis Files/Excel Files/2021_08.xlsx")
sep_2021 <- read_excel("Trip Data/Analysis Files/Excel Files/2021_09.xlsx")
```

# Step 2: (Clean Data)

## Inspect Data

``` r
compare_df_cols(oct_2020,nov_2020,dec_2020,jan_2021,feb_2021,mar_2021,apr_2021,may_2021,jun_2021,jul_2021,aug_2021,sep_2021, return = 'mismatch')
```

    ##        column_name oct_2020 nov_2020  dec_2020  jan_2021  feb_2021  mar_2021
    ## 1   end_station_id  numeric  numeric character character character character
    ## 2 start_station_id  numeric  numeric character character character character
    ##    apr_2021  may_2021  jun_2021  jul_2021  aug_2021  sep_2021
    ## 1 character character character character character character
    ## 2 character character character character character character

## Since october and november have start_station_df and end_station_id as numeric data type, we need to convert them to character so that they can be correctly bound into a single dataframe

``` r
oct_2020 <- mutate(oct_2020, start_station_id = as.character(start_station_id),
              end_station_id = as.character(end_station_id))
nov_2020 <- mutate(nov_2020, start_station_id = as.character(start_station_id),
              end_station_id = as.character(end_station_id))
```

## Check for column data type mismatch again

``` r
compare_df_cols(oct_2020,nov_2020,dec_2020,jan_2021,feb_2021,mar_2021,apr_2021,may_2021,jun_2021,jul_2021,aug_2021,sep_2021, return = 'mismatch')
```

    ##  [1] column_name oct_2020    nov_2020    dec_2020    jan_2021    feb_2021   
    ##  [7] mar_2021    apr_2021    may_2021    jun_2021    jul_2021    aug_2021   
    ## [13] sep_2021   
    ## <0 rows> (or 0-length row.names)

## Stack individual dataframes into a master dataframe

``` r
master_df <- rbind(oct_2020,nov_2020,dec_2020,jan_2021,feb_2021,mar_2021,apr_2021,may_2021,jun_2021,jul_2021,aug_2021,sep_2021)
```

## Check column names

``` r
colnames(master_df)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"      "ride_length"        "day_of_week"

## Select relevant columns

``` r
master_df <- subset(master_df, select = -c(start_lat,start_lng,end_lat,end_lng,ride_length,day_of_week,ride_length,day_of_week)) #I'm removing two columns (ride_length and day_of_week) which were calculated using spreadsheet functions for Excel Analyis.
```

## Check dataframe structure

``` r
str(master_df)
```

    ## tibble [5,136,261 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:5136261] "ACB6B40CF5B9044C" "DF450C72FD109C01" "B6396B54A15AC0DF" "44A4AEE261B9E854" ...
    ##  $ rideable_type     : chr [1:5136261] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at        : POSIXct[1:5136261], format: "2020-10-31 19:39:43" "2020-10-31 23:50:08" ...
    ##  $ ended_at          : POSIXct[1:5136261], format: "2020-10-31 19:57:12" "2020-11-01 00:04:16" ...
    ##  $ start_station_name: chr [1:5136261] "Lakeview Ave & Fullerton Pkwy" "Southport Ave & Waveland Ave" "Stony Island Ave & 67th St" "Clark St & Grace St" ...
    ##  $ start_station_id  : chr [1:5136261] "313" "227" "102" "165" ...
    ##  $ end_station_name  : chr [1:5136261] "Rush St & Hubbard St" "Kedzie Ave & Milwaukee Ave" "University Ave & 57th St" "Broadway & Sheridan Rd" ...
    ##  $ end_station_id    : chr [1:5136261] "125" "260" "423" "256" ...
    ##  $ member_casual     : chr [1:5136261] "casual" "casual" "casual" "casual" ...

``` r
dim(master_df)
```

    ## [1] 5136261       9

``` r
summary(master_df)
```

    ##    ride_id          rideable_type        started_at                 
    ##  Length:5136261     Length:5136261     Min.   :2020-10-01 00:00:06  
    ##  Class :character   Class :character   1st Qu.:2021-04-11 18:50:57  
    ##  Mode  :character   Mode  :character   Median :2021-06-21 18:01:31  
    ##                                        Mean   :2021-05-25 22:30:55  
    ##                                        3rd Qu.:2021-08-11 21:13:51  
    ##                                        Max.   :2021-09-30 23:59:48  
    ##     ended_at                   start_station_name start_station_id  
    ##  Min.   :2020-10-01 00:05:09   Length:5136261     Length:5136261    
    ##  1st Qu.:2021-04-11 19:15:05   Class :character   Class :character  
    ##  Median :2021-06-21 18:20:59   Mode  :character   Mode  :character  
    ##  Mean   :2021-05-25 22:51:33                                        
    ##  3rd Qu.:2021-08-11 21:33:57                                        
    ##  Max.   :2021-10-01 22:55:35                                        
    ##  end_station_name   end_station_id     member_casual     
    ##  Length:5136261     Length:5136261     Length:5136261    
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ## 

``` r
skim(master_df)
```

|                                                  |           |
|:-------------------------------------------------|:----------|
| Name                                             | master_df |
| Number of rows                                   | 5136261   |
| Number of columns                                | 9         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| character                                        | 7         |
| POSIXct                                          | 2         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

Data summary

**Variable type: character**

| skim_variable      | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:-------------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| ride_id            |         0 |          1.00 |   5 |  23 |     0 |  5136052 |          0 |
| rideable_type      |         0 |          1.00 |  11 |  13 |     0 |        3 |          0 |
| start_station_name |    523467 |          0.90 |   3 |  53 |     0 |      784 |          0 |
| start_station_id   |    523781 |          0.90 |   1 |  36 |     0 |     1288 |          0 |
| end_station_name   |    567268 |          0.89 |  10 |  53 |     0 |      781 |          0 |
| end_station_id     |    567501 |          0.89 |   1 |  36 |     0 |     1288 |          0 |
| member_casual      |         0 |          1.00 |   6 |   6 |     0 |        2 |          0 |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| started_at    |         0 |             1 | 2020-10-01 00:00:06 | 2021-09-30 23:59:48 | 2021-06-21 18:01:31 |  4103233 |
| ended_at      |         0 |             1 | 2020-10-01 00:05:09 | 2021-10-01 22:55:35 | 2021-06-21 18:20:59 |  4095094 |

## Since we removed ride_length and day_of_week to show how they are done with the help of R, we add columns to indicate the day, month, year for each record

``` r
master_df$date <- as.Date(master_df$started_at)
```

### Use strftime funtion to strip year, month and day from date column we created

``` r
master_df$year <- strftime(master_df$date, "%Y")
master_df$month <- strftime(master_df$date, "%m")
master_df$day_of_month <- strftime(master_df$date, "%d")
master_df$day_of_week <- strftime(master_df$date, "%A")
```

### Add start_hour and end_hour from started_at and ended_at colums using strftime

``` r
master_df$start_hour <- strftime(master_df$started_at, "%H")
master_df$end_hour <- strftime(master_df$ended_at, "%H")
```

### Add ride_length column using difftime

``` r
master_df$ride_length <- difftime(master_df$ended_at,master_df$started_at)
```

### Inspect New Column

``` r
is.numeric(master_df$ride_length)
```

    ## [1] FALSE

### Convert ride_length to numeric for calcualtions

``` r
master_df$ride_length <- as.numeric(as.character(master_df$ride_length))
is.numeric(master_df$ride_length)
```

    ## [1] TRUE

## Check new column

``` r
summary(master_df$ride_length)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -1742998      424      756     1237     1369  3356649

### ride_length cannot be negative, select rows which are positive (new master dataframe will be created since we will be removing a lot of rows)

### remove start station name for trips where bikes were taken away for quality check

``` r
master_df_2 <- master_df[!(master_df$start_station_name == "HQ QR" | master_df$ride_length<0),]
```

## Inspect New Dataframe

``` r
summary(master_df_2)
```

    ##    ride_id          rideable_type        started_at                 
    ##  Length:5133521     Length:5133521     Min.   :2020-10-01 00:00:06  
    ##  Class :character   Class :character   1st Qu.:2021-04-09 05:50:12  
    ##  Mode  :character   Mode  :character   Median :2021-06-20 09:14:17  
    ##                                        Mean   :2021-05-24 15:10:30  
    ##                                        3rd Qu.:2021-08-10 18:54:29  
    ##                                        Max.   :2021-09-30 23:59:48  
    ##                                        NA's   :523409               
    ##     ended_at                   start_station_name start_station_id  
    ##  Min.   :2020-10-01 00:05:09   Length:5133521     Length:5133521    
    ##  1st Qu.:2021-04-09 06:19:47   Class :character   Class :character  
    ##  Median :2021-06-20 09:47:42   Mode  :character   Mode  :character  
    ##  Mean   :2021-05-24 15:33:55                                        
    ##  3rd Qu.:2021-08-10 19:12:18                                        
    ##  Max.   :2021-10-01 22:55:35                                        
    ##  NA's   :523409                                                     
    ##  end_station_name   end_station_id     member_casual           date           
    ##  Length:5133521     Length:5133521     Length:5133521     Min.   :2020-10-01  
    ##  Class :character   Class :character   Class :character   1st Qu.:2021-04-09  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :2021-06-20  
    ##                                                           Mean   :2021-05-24  
    ##                                                           3rd Qu.:2021-08-10  
    ##                                                           Max.   :2021-09-30  
    ##                                                           NA's   :523409      
    ##      year              month           day_of_month       day_of_week       
    ##  Length:5133521     Length:5133521     Length:5133521     Length:5133521    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##   start_hour          end_hour          ride_length     
    ##  Length:5133521     Length:5133521     Min.   :      0  
    ##  Class :character   Class :character   1st Qu.:    431  
    ##  Mode  :character   Mode  :character   Median :    764  
    ##                                        Mean   :   1404  
    ##                                        3rd Qu.:   1382  
    ##                                        Max.   :3356649  
    ##                                        NA's   :523409

### Removing rows with NA across all columns

``` r
master_df_2 <- na.omit(master_df_2)
```

# Step 4: Descriptive Analysis

``` r
summary(master_df_2$ride_length)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0     432     763    1359    1380 3356649

## Compare casual riders to members

``` r
aggregate(master_df_2$ride_length ~ master_df_2$member_casual, FUN = mean)
```

    ##   master_df_2$member_casual master_df_2$ride_length
    ## 1                    casual               2012.2033
    ## 2                    member                821.7529

``` r
aggregate(master_df_2$ride_length ~ master_df_2$member_casual, FUN = median)
```

    ##   master_df_2$member_casual master_df_2$ride_length
    ## 1                    casual                    1032
    ## 2                    member                     605

``` r
aggregate(master_df_2$ride_length ~ master_df_2$member_casual, FUN = max)
```

    ##   master_df_2$member_casual master_df_2$ride_length
    ## 1                    casual                 3356649
    ## 2                    member                  573467

``` r
aggregate(master_df_2$ride_length ~ master_df_2$member_casual, FUN = min)
```

    ##   master_df_2$member_casual master_df_2$ride_length
    ## 1                    casual                       0
    ## 2                    member                       0

### Casual riders ride more on average, and we can now check the average ride length by days (Order the days first so that it’s easier)

``` r
master_df_2$day_of_week <- ordered(master_df_2$day_of_week, levels = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))
```

### Average daily ride time for both members and casual riders

``` r
aggregate(master_df_2$ride_length ~ master_df_2$member_casual + master_df_2$day_of_week, FUN = mean)
```

    ##    master_df_2$member_casual master_df_2$day_of_week master_df_2$ride_length
    ## 1                     casual                  Sunday               2322.3825
    ## 2                     member                  Sunday                935.9325
    ## 3                     casual                  Monday               1992.6060
    ## 4                     member                  Monday                787.6146
    ## 5                     casual                 Tuesday               1802.5275
    ## 6                     member                 Tuesday                774.8611
    ## 7                     casual               Wednesday               1752.3923
    ## 8                     member               Wednesday                777.8053
    ## 9                     casual                Thursday               1723.5687
    ## 10                    member                Thursday                771.1657
    ## 11                    casual                  Friday               1931.4637
    ## 12                    member                  Friday                808.7012
    ## 13                    casual                Saturday               2166.8320
    ## 14                    member                Saturday                917.3571

## Let’s visualize the average ride duration by rider type on each day of the week

``` r
master_df_2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(rides = n(),
            average_ride_duration = mean(ride_length)) %>% 
  arrange(member_casual,day_of_week) %>% 
  ggplot(aes(x=day_of_week,y=average_ride_duration,fill=member_casual)) +
  geom_col(position = 'dodge')
```

![](Cyclistic_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

## Let’s visualize the number of rides by rider type on each day of the week

``` r
master_df_2 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x=day_of_week,y=number_of_rides, fill=member_casual)) +
  geom_col(position = 'dodge')
```

![](Cyclistic_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

## We can aggregate daily ride, average duration of rides and number of rides

``` r
master_df_2 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarize(number_of_ride=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,day_of_week)
```

    ## # A tibble: 14 × 4
    ## # Groups:   member_casual [2]
    ##    member_casual day_of_week number_of_ride average_duration
    ##    <chr>         <ord>                <int>            <dbl>
    ##  1 casual        Sunday              381857            2322.
    ##  2 casual        Monday              219120            1993.
    ##  3 casual        Tuesday             203624            1803.
    ##  4 casual        Wednesday           208598            1752.
    ##  5 casual        Thursday            222594            1724.
    ##  6 casual        Friday              278750            1931.
    ##  7 casual        Saturday            449435            2167.
    ##  8 member        Sunday              295714             936.
    ##  9 member        Monday              324110             788.
    ## 10 member        Tuesday             351391             775.
    ## 11 member        Wednesday           366120             778.
    ## 12 member        Thursday            362115             771.
    ## 13 member        Friday              345941             809.
    ## 14 member        Saturday            342045             917.

# Now we can export file for furter analysis

``` r
write.csv(master_df_2, "cleaned data.csv")
```
