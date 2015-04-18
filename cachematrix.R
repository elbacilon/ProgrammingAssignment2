# ----------------------Data Science > R-programming > Assigment 2-----------------

# Goal of the assignment is to write an R function able to cache potentially time-consuming 
# computations. For example, inversing a large square invertibale matrix could be 
# too long especially if it has to be computed repeatedly (e.g. in a loop). 
# If the contents of a square matrix are not changing, it may make sense 
# to cache the invers matrix so that when we need it again, it can be looked up 
# in the cache rather than recomputed. 

# In this Programming Assignment will take advantage of:
## 1/ The scoping rules of the R language and how they can be manipulated to preserve 
# state inside of an R object. 
## 2/ Solve function: which compute the inverse of a square matrix. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.

# Below are two functions that are used to create a special object that stores an
# invertible matrix and cache's its invers:
# 1/ makeCacheMatrix: This function creates a special "Vector" object which is 
# really a list containing a function to:
        # 1.set the value of the Matrix
        # 2.get the value of the MAtrix
        # 3.set the value of the Invers Matrix with Solve function
        # 4.get the value of the Invers Matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# 2/ cacheSolve: This function computes the inverse of the special "vector" 
# returned by makeCacheMatrix above. I# However, it first checks to see if the 
# mean has already been calculated. If so, it gets the invers matrix from the cache
# and skips the computation. Otherwise, it calculates the invers matrix of the data 
# and sets the value of the invers matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s) # sets the value of the invers matrix in the cache 
        s
}



