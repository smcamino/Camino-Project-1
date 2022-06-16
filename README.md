Project 1
================
Steph Camino
2022-06-08

# Required Packages

``` r
library(httr)
library(dplyr)
library(jsonlite)
library(tidyverse)
library(lubridate)
```

# NASA API Functions

# Basic Exploratory Data Analysis

## Data

## Tables

## Plots

r6dkgZPsg9wTctOy0pRVe3Ldhgk7mjcHZGuJMeNc

``` r
# Connecting to NASA API about Asteroid Sightings
APIData <- GET("https://api.nasa.gov/neo/rest/v1/feed?start_date=2022-06-01&end_date=2022-06-07&api_key=r6dkgZPsg9wTctOy0pRVe3Ldhgk7mjcHZGuJMeNc")
```

``` r
# Look for the element wanted, content
str(APIData, max.level = 1)
```

    ## List of 10
    ##  $ url        : chr "https://api.nasa.gov/neo/rest/v1/feed?start_date=2022-06-01&end_date=2022-06-07&api_key=r6dkgZPsg9wTctOy0pRVe3L"| __truncated__
    ##  $ status_code: int 200
    ##  $ headers    :List of 18
    ##   ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ all_headers:List of 1
    ##  $ cookies    :'data.frame': 0 obs. of  7 variables:
    ##  $ content    : raw [1:79772] 7b 22 6c 69 ...
    ##  $ date       : POSIXct[1:1], format: "2022-06-16 21:08:27"
    ##  $ times      : Named num [1:6] 0 0.000099 0.000102 0.00027 0.022808 ...
    ##   ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
    ##  $ request    :List of 7
    ##   ..- attr(*, "class")= chr "request"
    ##  $ handle     :Class 'curl_handle' <externalptr> 
    ##  - attr(*, "class")= chr "response"

``` r
# Grabbing List element we want and converts it to character
parsed <- fromJSON(rawToChar(APIData$content))

# Grabs a list that includes every date, which includes the data
near <- parsed$near_earth_objects
```

``` r
# This function is called when the user wants asteroid diameter in their dataset.
# The type of diameter units and whether they want minimum diameter, maximum diameter, or both diameters must be chosen. 
# Once diameter units and diameter choice is chosen, the function will grab it and insert it in the dataset.
asteroidDiameter <- function(diameterData, diameterUnits = "ft", diameterChoice = "both"){
  
  # Converts imputed strings for diameterUnit and diameterChoice to lowercase.
  diameterUnits <- tolower(diameterUnits)
  diameterChoice <- tolower(diameterChoice)
  
  # Conditional that grabs diameter depending on the type of unit chosen. 
  if(diameterUnits %in% "kilometers" || diameterUnits %in% "km"){
    
    diamUnit <- diameterData$kilometers
    
  } else if(diameterUnits %in% "meters" || diameterUnits %in% "m"){
      
      diamUnit <- diameterData$meters

    } else if(diameterUnits %in% "miles" || diameterUnits %in% "mi"){
        
        diamUnit <- diameterData$miles

      } else if(diameterUnits %in% "feet" || diameterUnits %in% "ft"){
          
          diamUnit <- diameterData$feet
          
        } else stop("DIAMETER UNITS: Units for diameter must be either kilometers, km, meters, m, miles, mi, feet, or ft")

  # Conditional that grabs diameter minimum, diameter maximum, or both depending on what was chosen.
  # This conditional uses the data grabbed from the above conditional, diamUnit. 
  if(diameterChoice %in% "min" || diameterChoice %in% "minimum"){
      
    diamChoice <- diamUnit$estimated_diameter_min
      
  } else if(diameterChoice %in% "max" || diameterChoice %in% "maximum"){
      
      diamChoice <-  diamUnit$estimated_diameter_max

    } else if(diameterChoice %in% "b" || diameterChoice %in% "both") {
        
        diamMin <- diamUnit$estimated_diameter_min
        diamMax <-  diamUnit$estimated_diameter_max
        diamChoice <- cbind(diamMin, diamMax) # Combines min and max variables

      } else stop("DIAMETER CHOICE: The choice for diameter must be minimum, min, maximum, max, both, or b")
  
  # Returns diameter
  return(diamChoice)                
  
}
```

``` r
asteroidData <- function(date = "01", id = "false", name = "false", magnitude = "false", diameter = "false", hazard = "false", diameterUnits, diameterChoice){
  
  # Conditional to grab data from the date chosen.
  # This date is the date the asteroid's were seen.
  # The date options are June 1, 2022 to June 7, 2022.
  if(date %in% "01" || date %in% "1"){
    dateData <- near$`2022-06-01`
    dateChosen <- "2022-06-01"
  } else if (date %in% "02" || date %in% "2") {
      dateData <- near$`2022-06-02`
      dateChosen <- "2022-06-02"
    } else if (date %in% "03" || date %in% "3"){ 
        dateData <- near$`2022-06-03`
        dateChosen <- "2022-06-03"
      } else if (date %in% "04" || date %in% "4") {
          dateData <- near$`2022-06-04`
          dateChosen <- "2022-06-04"
        } else if (date %in% "05" || date %in% "5") {
          dateData <- near$`2022-06-05`
          dateChosen <- "2022-06-05"
          } else if (date %in% "06" || date %in% "6") {
            dateData <- near$`2022-06-06`
            dateChosen <- "2022-06-06"
          } else if (date %in% "07" || date %in% "7") {
              dateData <- near$`2022-06-07`
              dateChosen <- "2022-06-07"
            } else {
                stop("DATE: use either 01, 1, 02, 2, 03, 3, 04, 4, 05, 5, 06, 6, 07, or 7")
              } 
     
  # Converts imputed function options to lowercase
  id <- tolower(id)
  name <- tolower(name)
  magnitude <- tolower(magnitude)
  diameter <- tolower(diameter)
  hazard <- tolower(hazard)
  
  # Creates empty dataset with the same number of rows as the data found above in the date conditional. 
  # This dataset will be used to insert all of the variables chosen for the final dataset.
  projectData <- data.frame(matrix(nrow = nrow(dateData)))
  
  # Creates new variable date with date chosen in every row. Converts string to a date too.
  projectData$date <- rep(ymd(dateChosen), nrow(dateData))
  
  # Conditional for Asteroid ID
  # If false or f is chosen, skip entire conditional.
  # If ID is chosen, add a new variable idData to projectData with all of the asteroid's IDs.
  if(!((id %in% "false") || (id %in% "f"))){
    if(id %in% "id"|| id %in% "idno"){
      projectData$idData <- dateData$id
    } else {
        stop("ID: use either false, f, id, or idno")
      }
  }
  
  # Conditional for Asteroid Name
  # If false or f is chosen, skip entire conditional.
  # If Name is chosen, add a new variable nameData to projectData with all of the asteroid's names. 
  if(!((name %in% "false") || (name %in% "f"))){
    if(name %in% "name"|| name %in% "n"){
      projectData$nameData <- dateData$name
    } else {
        stop("NAME: use either false, f, name, or n")
      } 
  }
  
  # Conditional for Asteroid absolute magnitude
  # If false or f is chosen, skip entire conditional.
  # If magnitude is chosen, add a new variable magData to projectData with all of the asteroid's absolute magnitude.  
  if(!((magnitude %in% "false") || (magnitude %in% "f"))){
    if(magnitude %in% "magnitude"|| magnitude %in% "m"){
      projectData$magData <- dateData$absolute_magnitude_h
    } else {
        stop("MAGNITUDE: use either false, f, magnitude, or m")
      }
  }
  
  # Conditional for Asteroid diameter
  # If false or f is chosen, skip entire conditional.
  # If diameter is chosen, grab the estimated diameter data from dateData, call the function ateroidDiameter, and add a new variable asterDiam or minDiam and max diam to projectData  depending on if min, max, or both is chosen with all of the asteroid's diameters. 
  if(!((diameter %in% "false") || (diameter %in% "f"))){
    if(diameter %in% "diameter"|| diameter %in% "d"){
      diameterData <- dateData$estimated_diameter
      asterDiam <- asteroidDiameter(diameterData, diameterUnits, diameterChoice)
      projectData <- cbind(projectData, asterDiam)
    } 
      else {
        stop("DIAMETER: use either false, f, diameter, or d")
      } 
  }  
  
  # Conditional for if the Asteroid is potentially hazardous
  # If false or f is chosen, skip entire conditional.
  # If Hazard is chosen, add a new variable hazardData to projectData with TRUE/FALSE if the asteroid is hazardous or not.   
  if(!((hazard %in% "false") || (hazard %in% "f"))){
    if(hazard %in% "hazard"|| hazard %in% "h") {
      projectData$hazardData <- dateData$is_potentially_hazardous_asteroid
    } else {
        stop("HAZARD: use either false, f, hazard, or h")
      }
  }
  
  # Deletes the first column, which is the empty column created when we created the empty dataset. 
  projectData <- subset(projectData, select = -1)
  
  # Returns projectData
  return(projectData)
}
```

``` r
# Grabs data from June 1, 2022 with the asteroid's ID, Name, Magnitude, Diameter, both min and mas, in feet, and if it is Hazardous or not.
juneFirst <- asteroidData(date = "01", id = "id", name = "name", magnitude = "m", diameter = "d", hazard = "h", diameterUnits = "ft", diameterChoice = "b")

# Grabs data from June 6, 2022 with the asteroid's ID, Name, Magnitude, Diameter, both min and mas, in feet, and if it is Hazardous or not.
juneSixth <- asteroidData(date = "06", id = "id", name = "name", magnitude = "m", diameter = "d", hazard = "h", diameterUnits = "ft", diameterChoice = "b")

# Combines data from June 1, 2022 and June 6, 2022
june <- rbind(juneFirst, juneSixth)

# Creates new variable that is the range of possible diameters of the asteroid
june$diamRange <- june$diamMax - june$diamMin

# Creates new variable that categorizes range in low, medium, and high. Reordered from high, medium, and low.
june$diamRangeCategory <- ifelse(june$diamRange < quantile(june$diamRange, 0.25), "low",
                                  ifelse(june$diamRange < quantile(june$diamRange, 0.75), "medium", "high"))
june$diamRangeCategory <- ordered(june$diamRangeCategory, levels = c("high", "medium", "low"))
```

``` r
table(june$hazardData)
```

    ## 
    ## FALSE  TRUE 
    ##    27     7

``` r
table(june$date, june$hazardData)
```

    ##             
    ##              FALSE TRUE
    ##   2022-06-01    14    4
    ##   2022-06-06    13    3

``` r
table(june$date, june$hazardData, june$diamRangeCategory)
```

    ## , ,  = high
    ## 
    ##             
    ##              FALSE TRUE
    ##   2022-06-01     2    2
    ##   2022-06-06     3    2
    ## 
    ## , ,  = medium
    ## 
    ##             
    ##              FALSE TRUE
    ##   2022-06-01     6    2
    ##   2022-06-06     7    1
    ## 
    ## , ,  = low
    ## 
    ##             
    ##              FALSE TRUE
    ##   2022-06-01     6    0
    ##   2022-06-06     3    0

``` r
summary(june$diamRange)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   32.55  121.04  198.02  584.48  578.89 5183.01

``` r
summary(june$magData)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   16.59   21.35   23.70   23.13   24.75   27.60

``` r
june %>% group_by(hazardData) %>%
     summarise(avg = mean(magData), med = median(magData), var = var(magData))
```

    ## # A tibble: 2 x 4
    ##   hazardData   avg   med   var
    ##   <lgl>      <dbl> <dbl> <dbl>
    ## 1 FALSE       23.8  24.3  6.80
    ## 2 TRUE        20.6  21.3  3.24

``` r
june %>% group_by(date, hazardData) %>%
     summarise(avg = mean(magData), med = median(magData), var = var(magData))
```

    ## `summarise()` has grouped output by 'date'. You can override using the `.groups` argument.

    ## # A tibble: 4 x 5
    ## # Groups:   date [2]
    ##   date       hazardData   avg   med   var
    ##   <date>     <lgl>      <dbl> <dbl> <dbl>
    ## 1 2022-06-01 FALSE       24.2  24.4 7.49 
    ## 2 2022-06-01 TRUE        21.3  21.4 0.197
    ## 3 2022-06-06 FALSE       23.4  24.3 6.28 
    ## 4 2022-06-06 TRUE        19.7  21.2 7.34

``` r
g <- ggplot(data = june, aes(x = diamRangeCategory))
g + geom_bar(aes(fill = hazardData), position = "dodge") + 
  labs(x = "Diameter Range (ft)", title = "Can We Predict Asteroid Diameter When it's Hazardous?") +
  scale_x_discrete(labels = c("High", "Medium", "Low")) +
  scale_fill_discrete(name = "Hazardous", labels = c("No", "Yes"))
```

![](~/images/unnamed-chunk-217-1.png)<!-- -->

``` r
g2 <- ggplot(june, aes(x = magData))
g2 + geom_histogram(color = "black", fill = "red", size = 1, binwidth = 2) + 
  labs(x = "Magnitude (au)", title = "Histogram of Asteroid's Absolute Magnitude")
```

![](~/images/unnamed-chunk-218-1.png)<!-- -->

``` r
g2 + geom_histogram(aes(y = ..density.., fill = diamRangeCategory)) +
  geom_density(adjust = 0.5, alpha = 0.5, aes(fill = diamRangeCategory)) +
  labs(x = "Absolute Magnitude (au)", title = "Histogram + Kernel Smoother of an Asteroid's Magnitude by Diameter Range") +
  scale_fill_discrete(name = "Diameter Range (ft)", labels = c("High", "Medium", "Low"))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](~/images/unnamed-chunk-218-2.png)<!-- -->

``` r
g3 <- ggplot(june, aes(x = magData, y = diamRange))

g3 + geom_point() + 
  geom_smooth() + 
  labs(x = "Absolute Magnitude (au)", y = "Diameter Range (ft)", title = "Astroid Magnitude by Range") 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](~/images/unnamed-chunk-219-1.png)<!-- -->

``` r
g3 + geom_text(aes(label = idData, angle = 90)) + 
  labs(x = "Absolute Magnitude (au)", y = "Diameter Range (ft)", title = "Astroid Magnitude by Range") 
```

![](~/images/unnamed-chunk-219-2.png)<!-- -->

``` r
g4 <- ggplot(june, aes(x = hazardData, y = magData))
g4 + geom_boxplot(fill = "blue", alpha = 0.5) + 
  labs(x = "Potentially Hazardous", y = "Absolute Magnitude (au)", title = "Is There a Relationship in Asteroid Magnitude and if it's Potentially Dangerous?")
```

![](~/images/unnamed-chunk-220-1.png)<!-- -->
