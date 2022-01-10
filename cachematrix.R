## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

##CacheMatrix.R
##looked at the course example to get it going --- https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/mowodw4rEemS0Q4U5mc4kg --- 
## There are two functions:
## **** makeCacheMatrix - create matrix and inverse cache - nothing else
## **** cacheSolve -  if the inverse matrix exists in the chache it will fetch it from the cache. If not it will calculate it and fetch it from makeCacheMatrix


## Create a function which takes a matrix named A as an argument
makeCacheMatrix <- function(x = matrix()) {
##the null inverse matrix
	invs <- NULL
	
##set function for the matrix
    set <- function(y)  {
        x <<- y
        invs <<- NULL
    }
##get function to return the matrix    
	get <- function() x
    
##the function to return the inverse matrix
	setinvs <- function(inverse) invs <<- inverse
    getinvs <- function() invs

##list for all defined functions for future requests    
	list(set = set,
         get = get,
         setinvs = setinvs,
         getinvs = getinvs)
}


## function cacheSolve:
## **** Checks if the inverse object is available and if inverse matrix exist its fetches it from makeCacheMatrix
## **** If not - it will compute and return the inverse matrix, stores it for next reequests, and gets the new inverse object

## if available it fetches the inverse of the matrix from makeCacheMatrix function
cacheSolve <- function(x, ...) {
    invs <- x$getinvs()

## checks to make sure the inverse has been has been calculated - if not fetches it, solves it and prints. If yes it displays it on screen. 
    if (!is.null(invs)) {
        message("sit back - fetching cached data")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data, ...)
    x$setinvs(invs)
    invs
}


#to test the code run this:
myk <- makeCacheMatrix(matrix(1:4,nrow = 2,ncol = 2))
myk$get()
myk$getinvs()
cacheSolve(myk)
cacheSolve(myk)
message("the end")
