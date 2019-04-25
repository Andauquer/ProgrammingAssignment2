# makeCacheMatrix returns a list of functions to set and get a matrix
# it also contains a pair of functions to set and get the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  # We first set the inverse of the matrix as a null object
  inverseMatrix <- NULL
  
  # This function should recieve a matrix to set it in x
  # It also sets the inverse of this matrix as NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  # This function simply returns the original matrix
  get <- function() x
  
  # This function recieves the inverse of the original matrix (x)
  # and sets it in inverseMatrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  # This function returns the inverse of the original matrix (x)
  getInverse <- function() inverseMatrix
  
  # When makeCacheMatrix is called (it should be assigned to a variable)
  # returns the above functions so they can be call later to set/get x
  # and the inverse of x
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve calculates and returns the inverse of a cached matrix, if this exists inside the
# makeCacheMatrix function. It does it, only if there is not already a cached
# inverse matrix, and also checks, if the already cached matrix (if exists) is not already
# the inverse matrix of the original matrix (x in makeCacheMatrix).

# This function should recieve an object containing the list of functions that returns 
# the makeCacheMatrix, this way we can use it's functions inside of cacheSolve
cacheSolve <- function(x, ...) {
  
  # First we get the value of the inverseMatrix in makeCacheMatrix
  inverseMatrix <- x$getInverse()
  
  # We also get the value of the original matrix, by using the get() function contained inside
  # of that object that recieved the list of functions returned by makeCacheMatrix
  matrix <- x$get()
  
  # Now, we check two things, first, if the inverse of the original matrix isn't NULL, if isn't
  # NULL, we also check if this inverse matrix isn't already the inverse of the original matrix,
  # we do this, so we don't have to recalculate again the same result (the inverse of x in 
  # makeCacheMatrix)
  if(!is.null(inverseMatrix) && isTRUE(all.equal(solve(inverseMatrix), matrix))) {
    # If both the abode conditions are TRUE, then we don't have to recalculate the inverse of
    # the matrix, and we just simply return it
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  
  # Now, if the inverse matrix, hasn't already been calculated (inverseMatrix is NULL), or if the
  # inverseMatrix that exists, isn't the inverse of x, meaning that a new matrix has been set to
  # x in makeCacheMatrix, then we calculat the inverse with the solve() function.
  inverseMatrix <- solve(matrix)
  
  # We use the setInverse() function stored in the object that recieved the list of functions 
  # returned by makeCacheMatrix, to set the inverse of the matrix
  x$setInverse(inverseMatrix)
  
  # We also return this inverse matrix
  inverseMatrix
}


# To make things easier, i decided to add some statements to test the above functions 

# I store the list of functions that makeCacheMatrix returns, inside of FUN
FUN <- makeCacheMatrix()

# I create a 4x4 matrix, with random numbers, so we can test if we can get its inverse.
FUN$set(matrix(rnorm(16), 4, 4))

# Let's see the created matrix 
FUN$get()

# We send FUN as parameter to cacheSolve, this way cacheSolve can make use of makeCacheMatrix's
# functions to get the created matrix and try to cache its inverse, finally, it will return the 
# inverse matrix to INV
INV <- cacheSolve(FUN)

# We see the inverse of the matrix, that is now contained in INV
INV

# We check if indeed inverseMatrix is the inverse of the original matrix just created above
isTRUE(all.equal(solve(FUN$getInverse()), FUN$get()))

# We call cacheSolve again, since inverseMatrix now exists, and the original matrix hasn't changed
# it should only return the inverse of the matrix with a message above 
cacheSolve(FUN)




