#----------------------------------------------------------------------------------------
# Table 1 Template
# Scott Zimmerman & Kristina Van Dang
# February 16, 2022
#----------------------------------------------------------------------------------------
# Description: Function templates for automatically making customized table 1s
#----------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)

#----------------------------------------------------------------------------------------
# f_format_cell
## Function to fill in cells
##     inputs:
##         data: a dataframe containing the variable you want to summarize
##         varName: a string of the name of the variable you want to summarize
##         value (discrete variables only): The value
##         type: the type of cell output to create (options: "prop", "mean")
##              "prop": Uses "value" to calculate the number and proportion of varName with the given value
##                      Formats output e.g. "1234 (56%)"
##              "mean": Outputs the mean and sd e.g. "1.2 (0.34)"
## Function to create cell text:
##     a "prop": proportion with a particular "value" (for discrete variables) or
##     a "mean": mean with sd (for continuous variables)
#----------------------------------------------------------------------------------------
f_format_cell <- function(data,varName,value=NA,type="prop"){
  if(type=="prop"){
    n_num <- sum(data[,varName]==value,na.rm=TRUE)
    n_denom <- nrow(data)
    perc <- round(n_num/n_denom*100,1) #Designate rounding here
    result <- paste0(n_num," (",perc,"%)")
  }else if (type=="mean"){
    m <- round(mean(data[[varName]],na.rm=TRUE),2)
    s <- round(sd(data[[varName]],na.rm=TRUE),2)
    result <- paste0(m," (",s,")")
  }
  return(result)
}

#----------------------------------------------------------------------------------------
# f_make_row
## A wrapper for f_format_cell
## All inputs except "data" are passed to f_format_cell
## "data" is used to create subsets of data for each column, which are then passed to f_format_cell
##     inputs:
##         data: a dataframe containing the variable you want to summarize
##         varName: a string of the name of the variable you want to summarize
##         value (discrete variables only): The value
##         type: the type of cell output to create (options: "prop", "mean")
##              "prop": Uses "value" to calculate the number and proportion of varName with the given value
##                      Formats output e.g. "1234 (56%)"
##              "mean": Outputs the mean and sd e.g. "1.2 (0.34)"
#----------------------------------------------------------------------------------------
f_make_row <- function(data, varName, value=NA,type="prop"){
  #For this example, I was creating 3 columns:
  ## a summary for each variable, and
  ## summaries of each variable stratified to exposed and unexposed

  #Create subsets of the data
  d_exposed <- data %>% filter(exposed==1)
  d_unexposed <- data %>% filter(exposed==0)

  #Create a row by creating each cell using the
  ## column-specific data set you want to summarize
  row <- c(f_format_cell(data,varName,value,type),
           f_format_cell(d_exposed,varName,value,type),
           f_format_cell(d_unexposed,varName,value,type))
  return(row)
}

#----------------------------------------------------------------------------------------
# f_table_one
## A wrapper for f_format_cell
## All inputs except "data" are passed to f_format_cell
## "data" is used to create subsets of data for each column, which are then passed to f_format_cell
##     inputs:
##         data: a dataframe containing the variable you want to summarize
##         varName: a string of the name of the variable you want to summarize
##         value (discrete variables only): The value
##         type: the type of cell output to create (options: "prop", "mean")
##              "prop": Uses "value" to calculate the number and proportion of varName with the given value
##                      Formats output e.g. "1234 (56%)"
##              "mean": Outputs the mean and sd e.g. "1.2 (0.34)"
#----------------------------------------------------------------------------------------
make_table_1 <- function(data){
  # Create an empty data frame for table 1 that will expect character (string) entries
  table1 <- data.frame(full=character(),
                       exposed=character(),
                       unexposed=character(),
                       stringsAsFactors = FALSE)

  #Manually create the counts row
  table1["n",] <- as.character(c(nrow(data), sum(data$exposed==1), sum(data$exposed==0)))

  #These  variables are binary, so we care about proportion with value=1
  #MPG is continuous:
  table1["MPG",] <- f_make_row(data,"mpg",type="mean")

  #cyl is categorical
  table1["CYL 4",] <- f_make_row(data,"cyl",value=4,type="prop")
  table1["CYL 6",] <- f_make_row(data,"cyl",value=6,type="prop")
  table1["CYL 8",] <- f_make_row(data,"cyl",value=8,type="prop")

  #vs is binary:
  table1["VS",] <- f_make_row(data,"vs",value=1,type="prop")

  return(table1)
}

#----------------------------------------------------------------------------------------
## Run it
#----------------------------------------------------------------------------------------
#Use mtcars example data set, use "am" as the exposure variable
d <- mtcars %>% rename(exposed = am)
table1 <- make_table_1(d)
