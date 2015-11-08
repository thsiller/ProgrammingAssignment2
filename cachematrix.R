## Put comments here that give an overall description of what your
## functions do

## Create a matrix structure, which can store his inverse
makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y){x<<-y; inverse_mat<<-NULL}
        get <- function(){x}
        set_inv <- function(y){inv <<- y}
        get_inv <- function(){inv}
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## Calculate and set the inverse of the cacheMatrix if not already cached. Finally, return the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(is.null(x$get_inv())){
                message('Calculate inverse')
                inv <- solve(x$get())
                x$set_inv(inv)
        }
        x$get_inv()
}


test_cache_matrix <- function(){
      
        # 1. Generate an arbitrary function
        # 2. Calculate its inverse
        # 3. The inverse times the original matrix must give the identity matrix (up to numerical accuracy)
        message('Create a matrix and invert it without sticking to our super matrix object.')
        hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
        h8 <- hilbert(8); h8
        sh8 <- solve(h8)
        print(round(sh8 %*% h8, 3))
        
        
        # 4. Let's do the same, with our special matrix. The inverse should be calculated
        message('Create and invert a matrix')
        mat <- makeCacheMatrix(h8)
        cacheSolve(mat)
        print(round(mat$get_inv() %*% mat$get()))
        
        # 5. Let's repeat inverting the matrix and checking the result, without creating a new instance of the matrix
        # The inverse should not be calculated anymore
        
        message('Invert the matrix again. Has the inverse been recalculated?')
        cacheSolve(mat)
        print(round(mat$get_inv() %*% mat$get()))    
        
        # 6. Check the results by eyeballing
}

# test_cache_matrix()


# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
#
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }




