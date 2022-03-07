#Function definition
f_function <- function(reqVar1,
                       reqVar2,
                       optVar1="defaultVal1",
                       optVar2="defaultVal2"){
  #Manipulate the inputs
  combined_number <- reqVar1 + reqVar2
  combined_string <- paste0(optVar1,optVar2)

  #Return something, in this case a list of the two new variables we created
  result <- list(
    number = combined_number,
    string  = combined_string
  )
  return(result)
}

#Function call
f_function(1,2) #Runs, but you can't access the result now
my_result <- f_function(1,2)

#When using optional variables in order, the option name doesn't need to be specified
## i.e. these are the same:
my_result2 <- f_function(1,2,"A")
my_result3 <- f_function(1,2,optVar1="A")

#When changing optional variables out of order, the option name needs to be specified
## Here optVar1 is left as the default
my_result4 <- f_function(1,2,optVar2="B")

#Here we change both optional inputs
## Order doesn't matter, so these are the same:
my_result5 <- f_function(1,2,optVar1="A",optVar2="B")
my_result6 <- f_function(1,2,optVar2="B",optVar1="A")
