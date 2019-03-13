## The pair of functions below are two steps in a process
## 1) makeCacheMatrix creates a list object haa contains the 
##      original matrix and can also cache the result
##      of calculating its inverse
## 2) cachesolve checks whether the list object already 
##      holds the inverse matrix. If it does, it takes
##      the value. If not, it pulls the data and calculates
##      the inverse.


## Take a matrix as input x. Creates another object (list)
## that can contain the matrix and the calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set <-function(y){
                xx<<-y
                i<<-NULL
        }
        get <- function() x
        setinvt <- function(solve) i<<-solve
        getinvt <- function() i
        list(set=set, get=get,
             setinvt=setinvt,
             getinvt=getinvt)
        
}


## Takes CacheMatrix object and will return the inverse matrix.
## It first checks the cache and will use that answer if 
## available.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of original if stored
        i<-x$getinvt()
        if(!is.null(i)){
                message("getting stored data")
                return(i)
        }
## Since inverse wasn't stored, get matrix and calculate,
## store, and return
        data<-x$get()
        i<-solve(data)
        x$setinvt(i)
        i
        }
