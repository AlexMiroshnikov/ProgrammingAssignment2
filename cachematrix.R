## See functions descriptions for overall description

## Returns a cache wrapper for a matrix
## @arg x matrix
## @return list

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInv <- function(solution) {
        inv <<- solution
    }
    
    getInv <- function() {
        inv
    }
    
    list(
        set = set,
        get = get,
        setInv = setInv,
        getInv = getInv
    )
}


## Calculates inverse matrix for a cache-wrapped matrix x
## @arg x cache-wrapped matrix, @see makeCacheMatrix()
## @arg ... <args for solve()>
## @return matrix

cacheSolve <- function(x, ...) {
    solution <- x$getInv()
    
    if (!is.null(solution)) {
        message('cache hit')
        return(solution)
    }
    
    matrix <- x$get()
    solution <- solve(matrix, ...)
    x$setInv(solution)
    solution
}

## Uncomment the following code to check the functionality
#
#m <- matrix(1:4, 2, 2)
#cm <- makeCacheMatrix(m)
#result <- cacheSolve(cm)
#result <- cacheSolve(cm)
#
#m <- matrix(c(1,2,0, 0,4,5, 0,2,3), nrow=3, ncol=3)
#cm <- makeCacheMatrix(m)
#result <- cacheSolve(cm)
#result <- cacheSolve(cm)