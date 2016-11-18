## makeCacheMatrix generates a matrix object that can store its inverse in a cache
## 'm' is a matrix that is used to initialize the object

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  # set assigns a new matrix to the object and clears any cached inverse
  set <- function(m2) {
    m <<- m2
    inv <<- NULL
  }
  # get returns the stored matrix; get-/setinverse modify the inverse cache value
  # (but do not calculate the inverse; this is done by 'cacheSolve')
  get <- function() m
  setinverse <- function(newinv) inv <<- newinv
  getinverse <- function() inv
  
  # The object itself is actually a list of these functions, which can be accessed
  # by using the $ operator (i.e., myMatrix$set(newMatrix))
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve calculates the inverse of a matrix created by 'makeCacheMatrix'.
## If a cached inverse already exists, it returns it.  If not, it calculates
## the inverse and then stores it in the cache
cacheSolve <- function(m) {
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("Returning cached inverse")
    return(inv)
  }
  m_matrix <- m$get()
  inv <- solve(m_matrix)
  m$setinverse(inv)
  inv
}