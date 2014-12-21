
## Typically, calculating inverse of a matrix is usually a fast operation.
## However, for matrices with long vectors, this may be both time-consuming 	
## and computing intensive. Generally, computing intensive actions are stored
## in a cache, assuming, that there is no change to input parametres. The below
## functions exhibit similar idea: The first function "makeCacheMatrix" serves the purpose 
## of creating a cache of matrix inverse; meanwhile, the second function "cacheSolve"
## first consults with the cache to confirm that there already exists an inverse of the passed matrix. 
## In the event, an entry was found in the cache, it simply returns that value to the caller;
## else, this function computes a new inverse and stores it in the cache for future reference.
## Following the lectures, the true intent of this exercise is to demonstrate lexical scoping.






## The function makeCacheMatrix returns a list or a special "matrix" containing the following functions:
## 
##	 1. set the value of the special matrix
##	 2. get the value of the special matrix		
##	 3. set the value of the inverse
##	 4. get the value of the inverse
##
##
##@param : a square matrix (programming specification assumes this matrix is invertible)	
##
##@return : This function returns a list of functions to the caller. 
##	    This list contains 4 other functions to both get/set the value in the cache 	
##	    and get/set value of a particular matrix.	
##
##
##


makeCacheMatrix <- function (x = matrix()) {
	
	## This is a simple assertion test. The conditional checks whether determinant
	## of the input matrix is not 0. Borrowing from elementary linear algebra, if the 
	## determinant of a matrix is not zero, than and only than, an inverse exists.
	## I utilized this source for reference : http://www.sosmath.com/matrix/inverse/inverse.html
	## Also, R det function is used to obtain the determinant.	

	if (det(x) != 0){
	
	stop("non-invertible matrix") ##program execution halts with the error message
	
	}	
		
		
	s <- NULL ## Variable s initially set to NULL; acts as a reference to solve/matrix inverse 
         
	# set function sets or writes the value of the special matrix 	
 	
	set <- function(y) {
							
						
			x <<- y		## Use of super-assignment <<- 
		
    		 	s <<- NULL	

	}

	#get function returns the value of the special matrix
	
	get <- function () x 
	
	#setsolve function mutates or stores the inverse

	setsolve <- function (solve) s <<- solve	
	
	#getsolve function accesses or returns the inverse
	
	getsolve <- function () s
	
	#Finally, this function returns a list of all above mentioned functions

	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## The function "cacheSolve" calculates the inverse of special "matrix" created with the above 
## makeCacheMatrix function.
## 
## First, the function consults the special matrix to ensure there exists an already computed inverse.	 ##	 
## Second, in the event one is found or chached, it simply returns with the message "getting cached data" to ## the caller	 
## Finally, at the lack there of, it computes the inverse with R solve function, and sets the value via 
## nested setmean function within makeCacheMatrix
##
##@param :	a square matrix to obtain its inverse, as created by makeCacheMatrix	
##@param : 	...., R ellipses act as input for solve()
##@return :	returns the inverse of input matrix 
##

cacheSolve <- function(x, ...) {


	i <- x$getsolve() ## variable i refers to the solved or inverse matrix
	
	
	## Assuming the inverse already exists in the cache, this conditional will yield truth value True
	
	if(!is.null(i)){

	message("getting cached data") ## A message to the caller that cached data will be returned
	return (i)

	}
	
	## At this point, the cached special matrix's inverse was not found. We obtain it via get and set 		## variable data to refer to it. The inverse is yet to be calculated.
	
	data <- x$get()
	
	## We compute the inverse via R's solve function, and assign the value to variable i.
	## The other set of input of parametres or ellipses (...) was for solve.

	i <- solve(data, ...) 
	
	

	x$setsolve(i) ## sets the value of the inverse into special matrix x via setsolve

	return(i)     ##returns the computed inverse




        
}
