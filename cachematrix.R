## This package contains two functions related to storing and solving
## a "matrix object," and caching the inverse of the matrix once calculated
## to minimize calculation time if the inverse is calculated more than once.

## makeCacheMatrix accepts an invertible matrix as is parameter, defaulting
## to an empty matrix. and associating four methods with the matrix.
##  set and get permit the replacement or retrieval of the matrix itself.
##  setInverse and getInverse enable the storing or retrieval of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    ##when creating a new object, inverse has not been calculated
    inverse <- NULL
    
    ##set method for replacing matrix in this object
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    ##get method for retrieving matrix
    get <- function() x
    
    ##stores the inverse of the matrix
    setInverse <- function(newInverse) inverse <<- newInverse
    
    ##retrieves stored inverse of the matrix
    getInverse <- function() inverse
    
    ##returns list of methods to enable interaction with the object
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve accepts a "matrix object" as its parameter, no default value.
##  The method will then solve the inverse of the matrix, starting by checking
##  for a cached inverse. If no cached value is found, the inverse of the matrix
##  is calculated and stored in the original object.

cacheSolve <- function(x, ...) {
    
    ##retrieve the stored value of the inverse from the object
    inverse <- x$getInverse()
    
    ##if the inverse is not null, return the stored value
    if (!is.null(inverse)){
        message("retrieving cached inverse of matrix")
        return(inverse)
    }
    
    ##else calculate and cache the inverse
    
    ##retrieve original matrix
    matrix <- x$get()
    
    ##calculate its inverse
    inverse <- solve(matrix)
    
    ##cache the inverse in the original object
    x$setInverse(inverse)
    
    ##return the inverse
    inverse
}
