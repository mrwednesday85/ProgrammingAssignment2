
##Create a matrix object that can cache its own inverse
makeCacheMatrix <- function(m = matrix()) {
        ##Set the matrix
        set<-function(matrix) {
                m<<-matrix
                i<<-NULL
        }
        ##Get the matrix
        get<-function() {
                ##Print the matrix on screen
                m
        }
        ## Set the inverse
        setinverse<-function(inverse){
                i<<-inverse
        }
        ## Get the inverse
        getInverse<-function(){
                ## Print the inverse
                i        
        }
        ## Return the method
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}

## Compute the inverse of the matrix returned by makecacheMatrix above.
## If the inverse has already been calculated, and the matrix has NOT changed, then the cachesolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<<-x$getInverse()
        ## Return inverse if it already exists
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## Get the matrix
        data<-x$get()
        ## Calculate the inverse of the matrix
        m<-solve(data)%*%data
        ## Set the inverse to the object
        x$SetInverse(m)
        ## Print the matrix
        m
}
