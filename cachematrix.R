## with these functions you can store a matrix 
## and calculate and store its inverse

## makeCacheMatrix takes a matrix and stores it in its local variable 'x'
## it creates another variable 'i'to store and initialize the inverse of 'x'
## 'i' is initialized with the placeholder value of 'NULL'
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve is inverting the matrix using the solve function
## it takes an object of 'type' makeCacheMatrix
## if getinverse() is 'NULL', i.e. missing, it calculates the inverse
## of the matrix and is setting x$setinverse() to the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}