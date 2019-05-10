## Coursera course "R Programming", 2nd course in the Johns Hopkins series "Data Science"
## Week 3 Programming Assignement
## Author: David M Larue
## Date: May 10, 2019

## Uses deep assignment to cache the inverse of a matrix

## Creates the cache for use by cacheSolve to cache the inverse of a matrix.
## The closure environment of set|get|setinverse|getinverse contains the matrix mat and its inverse inv

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## Solves for the inverse of a matrix whose value and cachre are created by makeCacheMatrix

cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'mat'
  inv<- mat$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinverse(inv)
  inv
}

#usage:
#
#> rm(list=ls()) # optional
#> source("cachematrix.R") # once per session
#> mat2<-matrix(c(4,3,2,1),2,2) # create a matrix
#> cm2<-makeCacheMatrix(mat2) # create the matrix's cache, put matrix in it
#> inv2<-cacheSolve(cm2) # solve the matrix, putting the matrix's inverse in the cache
#> inv2
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
#> cacheSolve(cm2) # solve the matrix again, this time retrieving the result from the cache
#getting cached data
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2

# closure environment
#
#> ls(environment(cm2$set))
#[1] "get"        "getinverse" "inv"        "mat"        "set"        "setinverse"
#> environment(cm2$set)$mat
#[,1] [,2]
#[1,]    4    2
#[2,]    3    1
#> environment(cm2$set)$inv
#[,1] [,2]
#[1,] -0.5    1
#[2,]  1.5   -2
#> environment(cm2$set)
#<environment: 0x00000000089249e0>

# remaining question: how to list all closure environments
