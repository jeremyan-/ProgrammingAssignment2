## Put comments here that give an overall description of what your
## functions do

## This function create a list of functions

makeCacheMatrix <- function(x = matrix()) {
      matrix <- NULL
      set <- function(y) {  # setting the value of the matrix
            x <<- y         # by calling this function and provide "y", it set the x to the matrix "y" to cache
            matrix <<- NULL
      }
      get <- function() x 
      set_inv <- function(inv_matrix) matrix <<- inv_matrix  # save inv_matrix into matrix 
      get_inv <- function() matrix # after inversed matrix is saved, this function gets the result
      list(set = set, get = get,  # return a list of four functions
           set_inv = set_inv,
           get_inv = get_inv)
}


## The following function first check whether inverse matrix is available by
## 1. getting the value 
## 2. if the value is NULL, it hasn't been calculated, then we will calculate it by
## 3. getting the data, and solve the matrix
## 4 if the value is not NULL, return the value from cache 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      matrix <- x$get_inv() # get the matrix from the object passed to this function
      if(!is.null(matrix)) {
            message("Getting cached inverted matrix...")  # if the matrix is not NULL, it means it is cached
            return(matrix) # then return the matrix
      }
      data <- x$get() # we will only do this if matrix is null. then we need to get the matrix
      #       print(data)
      inv_matrix <- solve(data) # after getting the matrix, solve it by inversing it, save.
      #       print(inv_matrix)
      x$set_inv(inv_matrix) # save the result into the cache
      inv_matrix # return the inversed matrix
}

# the following code were used to test whethet this works. 
m <- matrix(1:4, 2, 2)
matrix <- makeCacheMatrix(m)
cacheSolve(matrix)

exampleMatrix <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
matrixVector <- makeCacheMatrix(exampleMatrix)
matrixVector$get()
cacheSolve(matrixVector)
cacheSolve(matrixVector)
cacheSolve(matrix) 



