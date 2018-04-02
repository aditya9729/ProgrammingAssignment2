## These functions are used to compute the Matrix as well as its inverse
## 

##This function returns the matrix 

makeCacheMatrix <- function(x = matrix()) {    #function initialisation where x is a matrix 
  m <- NULL
  set <- function(y) {                         #set takes the matrix to be passed on
    x <<- y
    m <<- NULL
  }
  get <- function() x                           #get, returns the matrix
  setinv <- function(solve) m <<- solve         #setinv,one can compute and set the inv manually
  
  getinv <- function() m                        #getinv,this displays the inv matrix set originally by the user
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function calculates the inverse of a matrix where we used the solve(a,b,...)func
#here we pass matrix x in place of a ,since b isnt passed on ,it assumes b to be an identity matirx
#hence computing the inverse

cacheSolve <- function(x, ...) {              
  m <- x$getinv()                            #get inv from the list present 
  if(!is.null(m)) {                          # check and accordingly print 'getting cached data' and return the manually 
                                              #computed inverse matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
