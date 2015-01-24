## The function creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    a <- NULL
    
    set <- function(y) {

        if (!identical(x, y)) {
            ## reset cache for the changed matrix
            m <<- NULL
            a <<- NULL
        }
        x <<- y
    }
    get <- function() x
    
    ## getter and setter for the cached result
    setsolve <- function(sol) m <<- sol
    getsolve <- function() m
    
    ## getter and setter for agruments as function cacheSolve takes ...
    setargs <- function(args) a <<- args
    getargs <- function() a
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve,
         setargs = setargs,
         getargs = getargs)

}


## The function computes the inverse of the special "matrix" returned by `makeCacheMatrix`.
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    args <- match.call(expand.dots=T)
    
    ## get cached result
    m <- x$getsolve()
    
    ## get cached args
    a <- x$getargs()
    
    ## if cached data exists and args are the same than we can return cached data
    if(!is.null(m) 
       & !is.null(a) 
       & identical(a, args)) {
        message("getting cached data")
        return(m)
    }
    
    ## compute inverse
    data <- x$get()
    m <- solve(data, ...)
    
    ## save result in the cache
    x$setsolve(m)
    x$setargs(args)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
