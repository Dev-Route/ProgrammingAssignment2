
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    nvrs <- NULL
    set <- function(y){
        x <<- y
        nrvs <<- NULL
    }
    get <- function() x
    setnvrs <- function(inverse) nvrs <<- inverse
    getnvrs <- function() nvrs
    list(set = set, get = get, setnvrs = setnvrs, getnvrs = getnvrs)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    nvrs <- x$getnvrs()
    if(!is.null(nvrs)) {
        message("Getting Cached data")
        return(nvrs)
    }
    Neo <- x$get()
    nvrs <- solve(Neo, ...)
    x$setnvrs(nvrs)
    nvrs
}
