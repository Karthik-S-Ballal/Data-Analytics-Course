# Write a filter function to filter an atomic vector of characters (25 marks)

#A custom function to check for 1st character of each string in an atomic vector matches with the input provided

#filter_names1 function doesn't need lapply as substr vectorises and performs the extraction of string on its own
filter_names1 <- function(v,c){
  x <- substr(v,1,1) #Note: substr automatically vectorises and checks for each string in the input atomic vector
  x <- which(x == c)
  return (v[x])
}

#Below function that I used for the test performs the same operation but with using lapply
filter_names <- function(v,c){
     x <- lapply(v, function(a) {
          if (substr(a,1,1) == c) {
              return (a)
          }}) |> unlist()
     return (x)
}

v <- names(mtcars)
nv <- filter_names(v,"d")
nv

#Use lapply() to generate summary stats for variables in mtcars (25 marks)

target <- filter_names(names(mtcars),"c")
target

#Get mean of the columns that start with letter "c" 
s_d <- mtcars |>
  subset(select = target) |>
  lapply(function(x) mean(x)) |>
  unlist()
s_d

#Process and visualise the data from eirgrid17 (25 marks)
library(aimsir17)
library(ggplot2)
eirgrid17

#Date and IEWindGeneration has been selected on the basis of 1st month and year 2017
w_en_jan <- eirgrid17 |>
  subset(month == 1 &
           year == 2017,
         select=c(date, IEWindGeneration))
w_en_jan

#Graph plotted with blue points and lines joining them using geom_line
p1 <- ggplot(data = w_en_jan, mapping = aes(x =date, y = IEWindGeneration)) +geom_point(color="blue") +geom_line()
p1

#Process and visualise the data from observations (25 marks)

#Observations within months 1,11 and 12 in MACE HEAD station 
ob_mar_mh <- observations |>
             subset(month %in% c(1,11,12) &
                    station == "MACE HEAD",
                    select = c(date,station,msl,wdsp))
ob_mar_mh
#Head and Tail of the data (first and last 6 rows)
head(ob_mar_mh)
tail(ob_mar_mh)

#Minimum and maximum windspeed data
ws_min_max <- ob_mar_mh |>
              subset(wdsp %in% c(min(wdsp),max(wdsp)))
ws_min_max

#Plotting using ob_mar_mh along mean sea level and wind speed fitted with a geom_smooth (model = linear model)
p2 <- ggplot(data = ob_mar_mh, mapping = aes(x = msl, y = wdsp)) + geom_point() + geom_smooth(method = lm)
p2