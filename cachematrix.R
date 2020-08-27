## Task completion divided into 2 functions:
## makeCacheMatrix and cacheSolve

## First one gets a matrix and return list of functions, that can:
## 1)Assign matrix or reassign saved one to another(set)
## 2)Return assigned matrix(get)
## 3)Assign inverted matrix into environment(setInv) 
## 4)Return assigned inverted matrix(getInv)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {                         ## (1)
    m <<- y
    i <<- NULL
  }
  get <- function() m                          ## (2)
  setInv <- function(inverted) i <<- inverted  ## (3)
  getInv <- function() i                       ## (4)
  list(set = set, get = get,                   ## The list we return
       setInv = setInv,
       getInv = getInv)
}


## Second function gets a list, created by previous function and checks if
## there is inverted one in memory already.
## If there is no inverted matrix in environment, function gets matrix from
## list and checks if this matrix is invertible.
## If it is, assign it and return; If isn't, return message, that this
## matrix is not invertible.

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if (!is.null(i)){                  ## Checking if there is inverted one
    message('Getting cached data')   ## in environment
    return(i)
  }
  matrix <- x$get()                  ##Getting matrix from list
  if (det(matrix) != 0){             ##Checking if matrix is invertible
    i <- solve(matrix)
    x$setInv(i)                      ##Assigning inverted matrix
    i                                ##Returning inverted matrix
  }
  else {
    message('Matrix is not invertible')
  }
}
