
## Creates a matrix object that can cache its own inverse
makeCacheMatrix <- function( m = matrix() ) {
        
        ## Initialize the inverse property
        i <- NULL
        
        ## Set the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        ## Get the matrix
        get <- function() {
                ## Return the matrix
                m
        }
        
        ## Set the inverse
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## Get the inverse
        getInverse <- function() {
                ## Print the inverse
                i
        }
        
        ## List of methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of x
        m <- x$getInverse()
        
        ## Return the inverse if it's already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix
        data <- x$get()
        
        ## Calculate the inverse
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Print the matrix
        m
}