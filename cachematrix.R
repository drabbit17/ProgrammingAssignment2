## the two functions below allow to compute, starting from an invertible matrix, its inverse.
## Given that this operation may be time consuming in some cases. If the starting matrix
## has not changed it may make sense to cache its inverted form so that when its inverse is needed again
## it can be looked up in the cache rather than recomputed.

## The first function makeCacheMatrix() creates a list including 4 functions that allow to:
## 1) set the matrix value
## 2) get the matrix value
## 3) set the inverted matrix value
## 4) get the inverted matrix value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(mat) {
                x <<- mat
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverted) inv <<- inverted
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv )
}

## The following function calculates the inverse form of the matrix generated as output by the previous function
## before doing so, it also checks if the inverse has already been calculated, if it is the case it gets
## the inverse from the cache and skip the computation. Otherwise it calculate the inverse matrix and sets its value 
## in the cache through the setmean function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) # changed part
        x$setinv(inv)
        inv
}

# double check for the results
set.seed(118);  x <- matrix(rnorm(16, mean=5, sd=2), 4,4)       # randomly extract numbers for a matrix
matr <- makeCacheMatrix(x)      # create a list of functions including the starting matrix and the potentially manually inverted one
test <- cacheSolve(matr)        # compute the inverted matrix or recall it from the cache if this has been already computed in the past
round(test %*% x, 5) == diag(nrow = nrow(x), ncol = ncol(x)) # test for the function to have generated proper results

