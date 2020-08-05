## These functions can be used to cache the inverse of a matrix and pull an already calculated
## inverted matrix from the cache for future calculations. 

## This function creates a matrix object and cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set<-function(y){
                x<<- y
                s<<-NULL
        }
        get<- function() x
        setsolve<- function(solve) s<<-solve
        getsolve<- function() m
        list(set=set, get=get,setsolve=setsolve,getsolve=getsolve)
}


## This function computes the inverse of the matrix. If the inverse of the matrix has already been
## calculated the function will recall it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m<- mean(data,...)
        x$setsolve(m)
        m
}
