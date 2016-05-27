## Matrix functions to enable computing the inverse and caching the result

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inversedMatrix) m <<- inversedMatrix
  getInverseMatrix <- function() m
  list(set = set, 
       get = get, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache. 
## Assumes the matrix supplied is always invertible.
cacheSolve <- function(x, ...) 
{
  m <- x$getInverseMatrix()
  if(!is.null(m))
  {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverseMatrix(m)
  m
}
