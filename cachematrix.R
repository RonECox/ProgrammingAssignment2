# Student: Ronald E. Cox
# Course: R Programming
# Week: 2; Assignment: 2 - Lexical Scoping
# Function: cachematrix.R

# Compute the inverse of a square matrix and store result in a cache for future
# use when no changes have been made to original matrix

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Initialize 'm' variable and set to NULL, indicating no cached inverse
  set <- function(y) { #This function sets a new special "matrix" and NULLs 'm'
    x <<- y #Sets a new value for 'x' in the parent environment
    m <<- NULL #Sets 'm' to NULL, indicating no cached inverse
  }
  get <- function() x #This function gets value of special 'matrix'
  setinverse <- function(inverse) m <<- inverse #Function caches inverse in 'm'
  getinverse <- function() m #Function gets cached inverse from 'm'
  #Stores functions in a list and sets element names to match function names
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the
## matrix has not changed), then `cacheSolve` should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #Set 'm' to the value of 'm' in the 'makeCachMatrix' function
  if(!is.null(m)) { #If 'm' is NOT NULL
    message("getting cached data") #Print message
    return(m) #Return cached inverse
  } #If 'm' is NULL
  data <- x$get() #Get the updated value of the special 'matrix'
  m <- solve(data, ...) #Calculate new inverse and store in 'm'
  x$setinverse(m) #Update the cache with the new inverse
  m #Display the newly calculated inverse
}
