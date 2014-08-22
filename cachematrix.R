## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache. OK

makeCacheMatrix <- function(x = matrix()) {      #This is an OBJECT which takes in a matrix and returns a list of 4 function/methods (set, get, setinverse, and getinverse)
        m<-NULL
        
        ## Set Matrix
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x  # Gets the original matrix that you set when first calling makeCacheMatrix
        setinverse<-function(solve) m<<- solve
        getinverse<-function() m  ## Gets value of, which could be NULL or inverse of a matrix
        
        ## Return list of methods
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then ##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getinverse() ##Return matrix that is inverse of 'x'
        ## Return inverse if set already        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get() ## Get the matrix from object
        m<-solve(matrix, ...)  ## Calculate inverse of matrix
        x$setinverse(m)    ## this assigns the inverse of matrix 'x' to the variable 'm'
        m
}