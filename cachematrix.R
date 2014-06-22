## This functions allows to compute the inverse of a matrix and 
## cache it, so if we need it sometime later, doesn't need to
## compute again.

## 'makeCacheMatrix' is an object that store the matrix and the inverse
## and can be accessed via setting/getting methods

## 'cacheSolve' acts like a proxy of the solve function to compute 
## the inverse of the matrix stored in the 'makeCacheMatrix' object, or
## return its cache of this object if it's already computed



## 'makeCacheMatrix' makes an object to store and access to the matrix and
## its inverse via several methods

makeCacheMatrix <- function(x = matrix()) {
	## 'x' is the matrix that should be store.

	## Access methods:
	##
	## set (matrix): store a new matrix
	## get (): get the matrix stored
	## setinverse (matrix): set the inverse
	## getinverse(): get the inverse

	## Return a list of access functions that point to this object.


	## Initialize the 'inv' in order to store a future inverse of the matrix
	inv <- NULL

	## 'set' allows reinitilize the object, setting a new matrix 'x' and nulling 'inv'
	set <- function (y){
		x <<- y
		inv <<- NULL
	}

	## return the matrix 'x'
	get <- function () x

	## setting the inverse of the matrix 'x'
	setinverse <- function (inverse) inv <<- inverse

	## return the inverse of the matrix 'x'
	getinverse <- function () inv

	## return the list of the access functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## 'cacheSolve' return the inverse of the matrix stored in makeCacheMatrix, or 
## compute it. 

cacheSolve <- function(x, ...) {
	## 'x' is a makeCacheMatrix object that store a matrix

	## Return a matrix that is the inverse of 'x'


	## First, let's see if the inverse of 'x' is already computed
	inv <- x$getinverse()
	if(!is.null(inv)){
		## we got it!, return the inverse
		message("getting cached data")
		return (inv)
	}

	## At this point, inverse wasn't computed yet
	## get the matrix and store it in 'data'
	data <- x$get()
	## compute the inverse of the matrix 'data'
	inv <- solve(data, ...)
	## set the inverse in the makeCacheMatrix object 'x'
	x$setinverse(inv)
	## return the inverse
	inv

}


## Sorry about my English!!

