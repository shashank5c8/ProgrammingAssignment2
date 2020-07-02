## functions do

## Cache the matrix found and access globally

makeCacheMatrix <- function(x = matrix()) {
	inverse<-NULL
	set<-function(y){
		x<<-y
		inverse<<-NULL
	}
	get<-function() x
	setinverse<-function(inv) inverse<-inv
	getinverse<-function() inverse
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) 
}


## Find inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse<-x$getinverse()
	if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setmean(inverse)
        inverse
}
