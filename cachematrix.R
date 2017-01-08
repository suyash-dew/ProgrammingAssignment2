## the two functions combined will calculate and cache the inverse of a matrix
## to save the computation time

## this first function takes a matrix as input and returns a list
makeCacheMatrix <- function(x = matrix()) {
        invmat <- matrix()
        set <- function(y) {
                x <<- y
                invmat <<- matrix()
        }
        get <- function() x
        setinvmat <- function(solve) invmat <<- solve
        getinvmat <- function() invmat
        list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
}


## this function will calculate the inverse of a matrix 
## returned by the first function if the inverse of this
## matrix is never calculated before else it will obtain
## the inverse of the matrix from the cache

cacheSolve <- function(x, ...) {
        invmat <- x$getinvmat()
        sum <- 0
        if(nrow(invmat) == nrow(x$get()) & ncol(invmat) == ncol(x$get())){      #works as initial test to find if inverse of the martrix exists
## countinng the number of NAs, performing checks at each element to avoid warning message
                for(i in 1:nrow(invmat)){
                        for(j in 1:ncol(invmat)){
                                if(is.na(invmat[i][j]))
                                  sum <- sum + 1 
                        }
                }
                        if(sum > 0){
                        print("getting cached data")
                        return(invmat)
                }
        }
        mat <- x$get()
        invmat <- solve(mat,...)
        x$setinvmat(invmat)
        invmat
}

