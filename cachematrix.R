####################################################################
#
# Usage:
# 1. create a cache-able matrix using the function makeCacheMatrix
# 2. pass this cache-able matrix into the function cacheSolve
#    which retrieves an existing inverse, or creates a new one
#
####################################################################


# function to create a matrix which can also cache its inverse
# assumes that the matrix is invertible (no checks on invertibility)

makeCacheMatrix <- function (mtx = matrix()) {
   # local function checks for a square matrix
   isSquare <- function(m1) {
      stopifnot(nrow(m1) > 0, nrow(m1) == ncol(m1))
      return (TRUE)
   }
   
   # set up
   isSquare(mtx)
   inv <- NULL
   
   # getters
   get <- function() mtx
   getInverse <- function() inv
   
   # set a new matrix
   set <- function (mtx1) {
      if (isSquare(mtx1)) { # must be square
         if (nrow(mtx1) == nrow(mtx)) { # same number of rows as current matrix
            if (all.equal(mtx1, mtx)) { # check if entries are the same
               message("Same matrix as the current one")
               return # nothing to do
            }
         }
         # new square matrix
         mtx <<- mtx1
         inv <<- NULL
      }
   }
   
   # set the inverse
   setInverse <- function(inv1) {
      # make sure it is square, and same size as the matrix
      # one could do more integrity checks ...
      stopifnot(isSquare(inv1), nrow(inv1) == nrow(mtx))
      inv <<- inv1
   }
   
   # return the list of functions
   list(get = get, getInverse = getInverse,
        set = set, setInverse = setInverse)
}


# compute the inverse of a cacheMatrix unless it's already
# computed, in which case return the existing inverse

cacheSolve <- function(cacheMtx, ...) {
   invMtx <- cacheMtx$getInverse()
   if (!is.null(invMtx)) { # matrix hasn't changed
      print ("returning cached inverse")
      return (invMtx)
   }
   
   # else compute inverse and cache it
   invMtx <- solve(cacheMtx$get(), ...)
   cacheMtx$setInverse(invMtx)
   
   # return inverse
   invMtx
}