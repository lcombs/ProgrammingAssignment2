## The functions below compute the inverse of a square matrix. 
## Once you have computed the inverse once, you will receive a 
## message "getting cached data" and the computation will not be
## done again.


# makeCacheMatrix is a function that creates a special "matrix" object, 
# so you can then cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        
        matrx <- NULL
        
        set <- function(y) {
                x <<- y
                matrx <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) matrx <<- inverse
        
        getinverse <- function() matrx
        
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve is a function that computes the inverse of the special "matrix"
# that is returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then the cacheSolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
        
        matrx <- x$getinverse()
        
        if(!is.null(matrx)){
                message("getting cached data")
                return(matrx)
        }
        
        data <- x$get()
        
        matrx <- solve(data, ...) # solve computes inverse 
        
        x$setinverse(matrx)
        
        matrx
}

# An example of how to use the functions:

# my_matrix <- 
# makeCacheMatrix(x = matrix(runif(4, 1, 100), 2, 2))

# cacheSolve(my_matrix)
