## The script consists of makeCacheMatrix and cacheSolve functions

# creates an object that wraps a matrix with additional functions:
#       set - sets a new matrix value, clears cache
#       get - gets a matrix value
#       setinverse - sets an inversed matrix value
#       getinverse - gets an inversed matrix value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inversed) inv <<- inversed
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# computes the matrix's inverse value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


#################### test case follows ####################
# mymatrix <- rbind(c(1,2),1)
# cachedMatrix <- makeCacheMatrix(mymatrix)
# 
# cachedMatrix$get()
# #expected result:
# [,1] [,2]
# [1,]    1    2
# [2,]    1    1
# #
# 
# cachedMatrix$getinverse() #expected: NULL
# 
# cacheSolve(cachedMatrix)
# #expected result:
# [,1] [,2]
# [1,]   -1    2
# [2,]    1   -1
# 
# cacheSolve(cachedMatrix)
# #expected result, hitting the cache:
# getting cached data
# [,1] [,2]
# [1,]   -1    2
# [2,]    1   -1
# 
# cachedMatrix$getinverse()
# #expected result
# [,1] [,2]
# [1,]   -1    2
# [2,]    1   -1
# 
# 
# mymatrix2 <- rbind(c(1,2),2)
# cachedMatrix$set(mymatrix2)
# #expected result, matrix changed, cache is gone:
# cachedMatrix$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    2    2
# 
# cachedMatrix$getinverse() #expected result, matrix changed, cache is gone: NULL