#`makeCacheMatrix`: This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #always restore m to NULL when makeCacheMatrix is called

        #set the value of the matrix
                set <- function(y) {    # takes an input vector
                x <<- y         # saves the input vector 
                m <<- NULL      # resets the m to NULL, basically what happens when a new object is generated.
        }
        
        #  these next four  functions are defined but not run when makeCacheMatrix is called.
        #   instead, they will be used by cachesolve() to get values for x or for
        #   inv (inversematrix) and for setting inv (the inversematrix).  
        
        
        #get the value of the matrix
        get <- function(){ x} # this function returns the value of the original vector
        setinv <- function(solve) {m <<- solve} #  this is called by cachemean() during the first cachemean()
        #  access and it will store the value using superassignment "<<-"
        getinv <- function() {m}        # this will return the cached value to cachematrix() on
        #  subsequent accesses
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
         #  This is accessed each time makecachedmatrix() is called,       
         #   that is, each time we make a new object.  This is a list of 
         #   the internal functions ('methods') so a calling function
         #   knows how to access those methods.                            
        
}


## returns a previously cached inverse matrix, or failing that calculates the inverse afresh

cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
                        m <- x$getinv()               # accesses the object 'x' and gets the values the inverse matrix                if(!is.null(m)) {              # if mean was already cached (not NULL) ...
                        if(!is.null(m)) { #does the next bit if it can find a value for m
                        message("getting cached data")  # ... send this message to the console
                        return(m)                     # ... and return the inversematrix ... "return" ends 
                        #   the function cachesolve(). 
                }
                data <- x$get()        # we reach this code only if x$getinv() returned NULL
                m <- solve(data, ...)   # if m was NULL then we have to calculate the inversematrix
                x$setinv(m)           # store the calculated inverse matrix in x (see setinv() in CacheMatrix()
                m               # return the inverse to the code that called this function
        }

