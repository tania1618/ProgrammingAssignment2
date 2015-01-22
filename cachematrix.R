## The following functions are used to create a special "matrix" 
#  object that stores a matrix and caches its inverse.

## The makeCacheMatrix() creates a special "matrix" object
#  that can store a matrix through writematrix() function, and
#  cache its inverse through setinverse() function.
#  This special "matrix" that is returned by the makeCacheMatrix()
#  is in fact a list of four functions (see the list() below).

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        writematrix<-function(y){
                x<<-y
                inv<<-NULL
        }
        readmatrix<-function() x
        setinverse<-function(j) inv<<-j
        getinverse<-function() inv
        list(writematrix=writematrix, readmatrix=readmatrix,
             setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve() function firstly checks if the special "matrix" 
#  returned by the makeCacheMatrix() has cached the inverse
#  of the associated matrix (returned by the readmatrix()).

# If the inverse for that particular matrix has been already
# computed, then it is fetched from the cache,
# otherwise it is calculated using solve() function and then
# stored in the cache using the setinverse() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if (!is.null(inv)){
                message("the inverse has been already calculated")
                return(inv)
        }
        z<-x$readmatrix()
        inv<-solve(z)
        x$setinverse(inv)
        inv
}
