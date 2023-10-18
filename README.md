# pxR2 <img src="man/figures/logo.png" align="right" height="139"/>

The goal of pxR2 is to provide a faster and easier way to read and write px-data (PC-AXIS), inspired by the packages pxR and readr. It is designed to correctly parse data and metadata and easy integration with other px-related programs such as PXjob and PXEdit.

# Installation

In the near future, pxR2 will be on CRAN.

``` r
devtools::install_github('emilwest/pxR2')
```

# Examples

## Create px file from dataframe/tibble

``` r
library(pxR2)
ex_data
#> # A tibble: 27 × 5
#>    sex    age   time  value pxvalue
#>    <chr>  <chr> <chr> <dbl> <chr>  
#>  1 Female 0-15  2021   16.9 <NA>   
#>  2 Female 0-15  2022   20.9 .....  
#>  3 Female 0-15  2023   15.8 ....   
#>  4 Female 16-25 2021   28.0 <NA>   
#>  5 Female 16-25 2022   21.6 .....  
#>  6 Female 16-25 2023   15.9 .      
#>  7 Female 26-50 2021   22.4 <NA>   
#>  8 Female 26-50 2022   23.7 ....   
#>  9 Female 26-50 2023   22.9 ....   
#> 10 Male   0-15  2021   18.5 <NA>   
#> # ℹ 17 more rows
px_obj <- px_create(ex_data,
                    stub = "sex,age",
                    heading = "time",
                    time_variable = "time",
                    time_scale = "annual",
                    matrix = "TEST01",
                    subject_area = "Forestry",
                    subject_code = "F",
                    units = "Number",
                    contents = "Number of trees",
                    decimals = 1,
                    language = "en"
)

px_obj
#> $metadata
#> # A tibble: 20 × 5
#>    keyword       language varname valname value                                 
#>    <fct>         <chr>    <chr>   <chr>   <chr>                                 
#>  1 CHARSET       <NA>     <NA>    <NA>    "ANSI"                                
#>  2 AXIS-VERSION  <NA>     <NA>    <NA>    "2013"                                
#>  3 CODEPAGE      <NA>     <NA>    <NA>    "iso-8859-15"                         
#>  4 LANGUAGE      <NA>     <NA>    <NA>    "en"                                  
#>  5 CREATION-DATE <NA>     <NA>    <NA>    "20231018 17:03"                      
#>  6 DECIMALS      <NA>     <NA>    <NA>    "1"                                   
#>  7 SHOWDECIMALS  <NA>     <NA>    <NA>    "1"                                   
#>  8 MATRIX        <NA>     <NA>    <NA>    "TEST01"                              
#>  9 SUBJECT-CODE  <NA>     <NA>    <NA>    "F"                                   
#> 10 SUBJECT-AREA  <NA>     <NA>    <NA>    "Forestry"                            
#> 11 TITLE         en       <NA>    <NA>    "Number of trees by sex, age and time…
#> 12 CONTENTS      <NA>     <NA>    <NA>    "Number of trees"                     
#> 13 UNITS         <NA>     <NA>    <NA>    "Number"                              
#> 14 STUB          <NA>     <NA>    <NA>    "sex,age"                             
#> 15 HEADING       <NA>     <NA>    <NA>    "time"                                
#> 16 VALUES        <NA>     sex     <NA>    "\"Female\",\"Male\",\"Total\""       
#> 17 VALUES        <NA>     age     <NA>    "\"0-15\",\"16-25\",\"26-50\""        
#> 18 VALUES        <NA>     time    <NA>    "\"2021\",\"2022\",\"2023\""          
#> 19 TIMEVAL       <NA>     time    A1      "2021,2022,2023"                      
#> 20 LAST-UPDATED  <NA>     <NA>    <NA>    "20231018 17:03"                      
#> 
#> $data
#> # A tibble: 27 × 5
#>    sex    age   time  value pxvalue
#>    <chr>  <chr> <chr> <dbl> <chr>  
#>  1 Female 0-15  2021   16.9 <NA>   
#>  2 Female 0-15  2022   20.9 .....  
#>  3 Female 0-15  2023   15.8 ....   
#>  4 Female 16-25 2021   28.0 <NA>   
#>  5 Female 16-25 2022   21.6 .....  
#>  6 Female 16-25 2023   15.9 .      
#>  7 Female 26-50 2021   22.4 <NA>   
#>  8 Female 26-50 2022   23.7 ....   
#>  9 Female 26-50 2023   22.9 ....   
#> 10 Male   0-15  2021   18.5 <NA>   
#> # ℹ 17 more rows


# Save object

px_write(px_obj, "somepath/TEST01.px")
```



