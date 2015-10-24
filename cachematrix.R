## The following functions makeCacheMatrix and cacheSolve help to calculate
## the  inverse of the matrix and cache the value of the inverse matrix. If 
## a call to inverse the same matrix is invoked, the function retrieves the
## inversed matrix from the cache instead of recomputing. If a new matrix is 
## passed, the inverse of the matrix is computed.

## makeCacheMatrix: Input to the function is only a matrix. Creates a cache 
## object to store the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## new matrix that is input is saved using the
        ## set function
        set <- function(y){
          x <<- y
          m <<- NULL
        }
        ## get returns the saved matrix
        get <- function() x
        
        ## inversed matrix is saved using setInverse
        setInverse <- function(Inverse) m<<- Inverse
        
        ##returns the inversed matrix
        getInverse <- function() m
        
        ##returns the values of the functions as a list
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve: computes the inverse of a matrix returned by makeCacheMatrix. If
## inverse is already calculated( and the matrix has not changed), the cacheSolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        
        ## If inverse is already calculated and in the cache
        ## return the cached data and come out of the cacheSolve
        ## function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## get the matrix data for which inverse needs to be computed
        data <- x$get()
        ## calculate inverse using the solve function
        m <- solve(data,... )
        ## cache the  inversed matrix
        x$setInverse(m)
        ## return inversed matrix
        m
}


##Input to test.
z<-matrix(c(5,2,10,11,12,2,35,21,10),3)
z
s<-makeCacheMatrix(z)

cacheSolve(s)

cacheSolve(s) ## get the message "getting cached data" this time
z1<-matrix(c(5,2,23,11,12,2,35,21,10),3)
s1<-makeCacheMatrix(z1)

cacheSolve(s1)

