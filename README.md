# upsandowns package

The purpose of this package is to create a package of functions to assist with the writing of small sections of automated commentary inside RMarkdown documents, particularly in creating descriptions of data. All of the functions in this code are designed to work in any project, and are general functions that would be of use to a wide range of people.

## Installation

The package can be installed directly from Github using the remotes install_github call

```
install.packages("remotes")
remotes::install_github("department-for-transport-public/upsanddowns")
```

## Overview

### Numerical changes

The package has three different functions which operate in the same way, but are designed to accept slightly different numeric data types which represent changes in data. These are:

* `percent_change`: for percentage changes
* `pp_change`: for percentage point changes
* `number_change`: for any numeric change

All three will accept a numeric value or vector, and return a short string of commentary based on the value of the numeric. Negative values will be accompanied by a "down" word (decrease/down/etc) while positive values will be accompanied by an "up" word (increase/up/etc). Small changes (less than 1% or 1pp, or 100 in a numeric) will be recorded with a stable word (little change/etc.)

You can choose which descriptor words you would like to use by making use of the `description` argument and an associated code. By default, it is set to up and down. To see the full list of descriptor options and their codes, use the `show_descriptions()` function.

Additional arguments for the three functions include:

* `percent_change`: `...` for any additional arguments to be passed to the scales::percent function. 
* `pp_change`: `abbr` to indicate whether you'd like changes recorded as "percentage points" or "pp". 
* All functions: `unchanged_limit` to set the limit in either direction for what constitutes "no change". 
### Top values in a dataset

The package also has two different functions designed to describe the top *n* number of values in a dataset (containing values and names for those values), with each function returning a slightly different description based on the same data.

* `top_x`: returns a string of the *names* of the top "n" values, in a grammatically correct way. Will provide a warning if there are other equal values outside of the top n values. i.e. will return "north, south and east" for n = 3.
* `top_x_percent`: returns a string of the *names* of the top "n" values, alongside their percentage of the total values provided. i.e. will return "north, south and east (20% of total)" or "north, south and east (10%, 5% and 2% of total respectively)" for n = 3.

Arguments for these functions include:

* `n`: the number of values to report on in the commentary (e.g. n = 3 will return a commentary discussing the top 3 values)
* `values`: column name of the column containing numerical values to sort top n values on. 
* `names`: column name of the column containing name values used to describe the top n values. 
* For top_x_percent only: `sum_percentage` Do you want a single percentage of the total to be returned for all n number of values, or an an individual percentage for each value?
