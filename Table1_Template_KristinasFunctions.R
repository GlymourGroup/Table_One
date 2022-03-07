# Table 1 Template
# Scott Zimmerman & Kristina Van Dang
# February 16, 2022

#----------------------------------------------------------------------------------------

#This shows
f_format_cell <- function(data,varName,value=NA,type="prop"){
  if(type=="prop"){
    n_num <- sum(data[,varName]==value,na.rm=TRUE)
    n_denom <- nrow(data)
    perc <- round(n_num/n_denom,2) #Designate rounding here
    perc <- format(perc,nsmall=2) # keep the format #.## in printing
    s <- round(sd(data[[varName]],na.rm=TRUE),2)
    s <- format(s,nsmall=2)
    result <- paste0(perc," (",s,")")
  }else if (type=="mean"){
    m <- round(mean(data[[varName]],na.rm=TRUE),2)
    m <- format(m,nsmall=2)
    s <- round(sd(data[[varName]],na.rm=TRUE),2)
    s <- format(s,nsmall=2)
    result <- paste0(m," (",s,")")
  }
  return(result)
}

#make_row creates a row for each model by applying count_among to every cell of interest
f_make_row <- function(data,varName,value=NA,type="prop"){

  #For this example, I was creating 3 columns: black, latino, and white
  d_black <- data %>% filter(race==1) #black
  d_latino <- data %>% filter(race==2) #latino
  d_white <- data %>% filter(race==3) #white

  row <- c(f_format_cell(d_black,varName,value,type),
           f_format_cell(d_latino,varName,value,type),
           f_format_cell(d_white,varName,value,type))

  return(row)
}


f_make_table_1 <- function(data){
  # Create an empty data frame for table 1 that will expect character (string) entries
  table1 <- data.frame(black=character(),
                       latino=character(),
                       white=character())

  #Manually create the counts
  table1["n",] <- c(sum(data$race==1),sum(data$race==2),sum(data$race==3))

  #These  variables are binary, so we care about proportion with value=1
  table1["Age, years",] <- f_make_row(data,"Nage",1, type="mean")
  table1["Male",] <- f_make_row(data,"male",1, type="prop")
  table1["Spanish only",] <- f_make_row(data,"lang_span",1, type="prop")

  return(table1)
}

