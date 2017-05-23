
## This function
# Sets the value of Matrix using set()
# Gets the value of Matrix using get()
# Sets the value of Inverse using setinverse()
# Gets the value of inverse using getinverse()

makeCacheMatrix <- function(x = matrix()) 
{

        inv<- NULL
        set <- function(y)
                {
                x <<- y
                inv <<- NULL
                }       
        get <- function() x
        setinverse <- function(inverse) inv<<- inverse
        getinverse<- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## The following function calculates the inverse of the matrix created with the makeCacheMatrix(). 
# It first checks to see if the inverse has already been calculated. 
# If so, get the inverse from the cache and skip the computation. 
# If not calculate the inverse of the matrix data and set the value of 
# the inverse in the cache using the setinverse() function.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if (!is.null(inv)) 
                {
                message("Getting Cached Data")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
