makeVector <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() (x)
        setInverse <- function(inverse) (inv <<- inverse)
        getInverse <- function() (inv)
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        ##The sole purpose of this function is to generate a matrix and display it. Meanwhile, this function also calls the second function to get the inverse. 
        
}





cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
        ## This function is supposed to check the cache whether the inverse is already present or not.If it is present, then it displays the message that it is fetching cache details.Otherwise it processes and generates the inverse.
}