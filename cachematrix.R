## makeCacheMatrix function provides the required stack of functions
## to cache the original matrix and the inverse matrix.
## 
## cacheSolve function uses the above mentioned functionalities to
## save time and computational resources to solve the matrix.

## This function creates a list of functions to store and retrieve 
## the original matrix and the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL;
  
  set <- function(y) {
    if(is.matrix(y)) {
      if(dim(x) == dim(y) && all(x == y)) {
        x <<- y;
      } else {
        x <<- y;
        inv <<- NULL;
      }
    }
  }
  
  get <- function() x;
  
  setInv <- function(inverse) inv <<- inverse;
  
  getInv <- function() inv;
  
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv);
}


## Checks if the inverse matrix exists or not, if exits
## retrun it, if not solve the matrix and set the inverse matrix 
## in cache and return the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv();
  if(!is.null(inv)) {
    message("getting cached inverse matrix");
    return(inv);
  } else {
    message("Solving the matrix for inverse");
  }
  m <- x$get();
  inv <- solve(m);
  x$setInv(inv);
  inv;
}
