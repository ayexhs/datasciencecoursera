## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
    # erasing cached data
    m <- NULL
    
    # set value of matrix
    set <- function(y) {
        # <<- used to assign values to objects x and m in parent environment
        x <<- y 
        m <<- NULL
    }
    
    # get value of matrix
    get <- function() x # lexical scoping
    
    # set value of inverse matrix
    setinverse <- function(inverse) m <<- inverse
    
    # get value of inverse matrix
    getinverse <- function() m
    
    # named elements in list allows use of $ to access functions
    list(set = set, # gives name set to set()
         get = get, # gives name get to get()
         setinverse = setinverse, # gives name setinverse to setinverse()
         getinverse = getinverse # gives name getinverse to getinverse()
         )
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    # returns to parent env if cached matrix is valid
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    # executes if false
    data <- x$get()
    m <- inverse(data, ....)
    x$setinverse(data)
    m
}
