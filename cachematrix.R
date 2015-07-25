## makeCacheMatrix - creates a list consisting of matrix input, input "set" in another environment, mean of inverse, 
## mean of inverse "set" in another environment 
# before running this function, install the MASS package (for the ginv() function)
# set makeCacheMatrix(x) = to a variable
makeCacheMatrix <- function(x = matrix()) { #takes vector or matrix input
  m <- NULL
  set <- function(y) { # lexical scoping - sets copy of input to different environment
    x <<- y
    m <<- NULL
  }
  get <- function() ginv(x) #given x is a matrix, ginv(x) produces inverse
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, 
       setmean = setmean,
       getmean = getmean)
}

#returns the set variable, "gets" the variable (inverse of x)
# sets the value of mean(ginv(x)), gets the result

## cacheSolve - searches list input if mean was already computed, if so returns cached mean, if not calculates mean from input
## and returns the value (m)
# cacheSolve(a (the variable you set makeCacheMatrix(x)) = to)
cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) { #searches cache for calculated mean
    message("getting cached data")
    return(m)
  }
  data <- x$get() #if mean not calculated, "gets" x val from inital input
  m <- mean(data, ...) #calculates mean of input
  x$setmean(m)
  m #returns mean of matrix inverse
}
