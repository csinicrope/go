## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## This script uses two functions to cache the inverse of a matrix: 
## makeCatchematrix and CacheSolve. 

## Includes a ridiculous number of comments since R still stymies me!


## makeCacheMatrix creates a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL               # sets inverse to NULL
        set <- function(y) {    # takes an input matrix
                x <<- y         # saves the input matrix
                i <<- NULL      # resets the inverse to NULL
        }
        get <- function() x     # returns the value of the original matrix
        
        setinverse <- function(inverse) i <<- inverse   
                # called by cacheSolve() to store inverse in parent environment
        
        getinverse <- function() i # returns the cached value to cacheSolve()
        
        list(set = set, get = get, # accessed when makeCacheMatrix () is called,   
             setinverse = setinverse, #list of functions defined above
             getinverse = getinverse)  
}


## cacheSolve returns the inverse of the matrix. First, checks to see if the 
## inverse has been calculated. If yes, accesses result and skips calculation.
## If no, calculates inverse, stores the inverse in the cache, and returns 
## the inverse. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()     # accesses 'x' and gets inverse
        if(!is.null(i)) {       # if i was already cached (not NULL)
                message("getting cached data")  #sends message to the console
                return(i)       # returns inverse.
        }
        data <- x$get()         # accesses if x$getinverse () returned NULL
        i <- solve(data, ...)   # if i was NULL, calculates inverse with solve()
        x$setinverse(i)         # stores inverse in makeCacheMatrix
        i                       # returns inverse to code
}

## Test1: Create matrix, then pass
x <- rbind(c(1,2), c(3, 4))
class(x)
x
mat <- makeCacheMatrix(x)
cacheSolve(mat) 
cacheSolve(mat) 

## Test 2: Pass matrix in first line, and pass
mat2 <- makeCacheMatrix(matrix(c(1, 3, 2, 4), nrow = 2))
mat2$get()
class(mat2$get())
cacheSolve(mat2) 
cacheSolve(mat2)
