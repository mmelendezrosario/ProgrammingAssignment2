
#The first function,  makeCacheMatrix  creates a special "matrix", 
# which is really a list containing a function to
     #1. set the value of the vector
     #2. get the value of the vector
     #3. set the value of the mean
     #4. get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
        inve = NULL
        set = function(y) { 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinve = function(inverse) inv <<- inverse 
        getinve = function() inve
        list(set=set, get=get, setinve=setinve, getinve=getinve)
}


##The following function calculates the mean of the special "matrix" created 
##with the above function. However, it first checks to see if the inverse 
##has already been calculated. If so, it  get s the inverse from the cache
##and skips the computation. Otherwise, it calculates the inverse of the data 
##and sets the inverse of the matrix in the cache via the  setinve  function.


cacheSolve <- function(x, ...) {     
        inv = x$getinve()
        if (!is.null(inve)){
                message("getting cached data")
                return(inve)
        }
        mat.data = x$get()
        inve = solve(mat.data, ...)      
        x$setinve(inve)
        return(inve)
}
Enter file contents here
