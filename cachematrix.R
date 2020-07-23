R Programming Project 2

Function:
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

Testing my function:

> pmatrix <-makeCacheMatrix(matrix(1:16, nrow=4, ncol=4))
> pmatrix$get()
     [,1] [,2] [,3] [,4]
[1,]    1    5    9   13
[2,]    2    6   10   14
[3,]    3    7   11   15
[4,]    4    8   12   16
> pmatrix$getInverse()
NULL
> pmatrix <-makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
> pmatrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> pmatrix$getInverse()
NULL
> cacheSolve(pmatrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> pmatrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> pmatrix$set(matrix(c(2,2,1,4),2,2))
> pmatrix$get()
     [,1] [,2]
[1,]    2    1
[2,]    2    4
> pmatrix$getInverse()
NULL
> cacheSolve(pmatrix)
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> cacheSolve(pmatrix)
getting cached data
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> pmatrix$getInverse()
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333

> 
