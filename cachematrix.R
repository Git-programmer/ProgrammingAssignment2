## Function to store and return a matrix and its inverse. Function to find inverse when not in cache 



## Returns a list of functions that input the matrix (set), output the matrix (get), input the inverse and output inverse respectively

makeCacheMatrix<-function(x = matrix()) { 
    inv<-matrix()
    ## Sets the matrix x to be the matrix passed by the set function. Erases the stored inverse that's in the cache
    set <- function(y) {  
        x <<- y 
        inv<<-matrix()
    }                  
    get <- function() x 
    set_inv <- function(inverse) inv <<- inverse 
    get_inv <- function() inv 
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

##Gets the inverse from the Cache. If found, returns it and exits otherwise computes the inverse using solve

cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    if(!is.na(inv)) { 
        message("getting cached inverse")
        return(inv)
    }
    Matrix <- x$get()
    Matrix_inverse <- solve(Matrix)
    x$set_inv(Matrix_inverse)
    Matrix_inverse
}

