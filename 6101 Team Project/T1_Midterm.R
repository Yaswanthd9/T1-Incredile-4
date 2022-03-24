# Libraries 
library (tidyverse)
library (dplyr)

# Lets take a look at our data

summary (heart_2020_cleaned)
nrow (heart_2020_cleaned)

# Creating a function that will allow us 
basic_eda <-  function(heart_2020_cleaned){
              glimpse (heart_2020_cleaned)
              print(status(heart_2020_cleaned))
              freq(heart_2020_cleaned) 
              print(profiling_num(heart_2020_cleaned))
              plot_num(heart_2020_cleaned)
              describe(heart_2020_cleaned)}

basic_eda(heart_2020_cleaned)

# We dont have an missing data since we downloaded clean data 

# Renaming data frame 

heart <- rename (heart_2020_cleaned)

# Change character variables to binary 

