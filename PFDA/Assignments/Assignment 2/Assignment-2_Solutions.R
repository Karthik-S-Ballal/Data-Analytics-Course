#Lab Assignment 2

#Set seed to 100 and a sample vector
set.seed(100)
v <- sample(1:100, 30)
v

##Write your own function to filter an atomic vector (25 marks):

#Custom function to filter out elements of an atomic vector based on modulus
filter_numbers <- function(v, m) {
  if (m == 0)
    stop("Make sure the input is a whole number greater than 0")
  sol <- c()
  
  return (v[v%%m == 0])
 }
#Few Examples
filter_numbers(v,2)
filter_numbers(v,5)
filter_numbers(v,10)
filter_numbers(v,0)

##Write your own functional to process an atomic vector (25 marks):

#Custom Function to process an atomic vector by applying function which is 
#passed as an argument

set.seed(100)
v <- sample(1:100,10)
v

process_vector <- function(v, fn) {
  return(fn(v))
}

#Few examples:
process_vector(v,mean)
process_vector(v,sd)


##Process the list sw_films (25 marks)

#Process the list sw_films
new_sw_films <- lapply(sw_films, function(x) 
  list(episode_id=x$episode_id,
       title=x$title,
       director=x$director,
       release_date=x$release_date))

str(new_sw_films)

#Updating the names for the new_sw_films:
names(new_sw_films) <- paste0("episode_id_",lapply(new_sw_films, function(x) x$episode_id))
str(new_sw_films)
##Get the movies for a director (25 marks):
#Custom function to get the title of the movies from the given data set
get_movies <- function(data, target) {
  movies <- data |> 
            lapply(function(x) {
              if (x$director == target) 
                x$title
            }) |> 
            unlist()
  return (movies)
}

#Few Examples:
mov1 <- get_movies(sw_films,"George Lucas")
mov1

mov2 <- get_movies(new_sw_films,"Richard Marquand")
mov2