#[makeCacheMatrix function creates an array object, which has 
#its inverse function calculated by cacheSolve function]

#Creates an array object and its manipulation functions 
#(get, set, getinverse, setinverse, etc.)
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#cacheSolve function returns the inverse of a determined 
#matriz
#If the inverse matrix is already available in the cache, the 
#function returns this value 
#without redoing the calculation. Otherwise, calculate the inverse of 
#the given matrix.
cacheSolve <- function(x, ...) {
        inv_x <- x$getinverse()
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setinverse(inv_x)
                return(inv_x)
        }
}
