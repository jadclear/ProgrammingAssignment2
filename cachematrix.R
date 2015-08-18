## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
    get<-function()x
	inv<-function(solve)m<<-solve
	ginv<-function()m
	list(set=set,get=get,inv=inv,ginv=ginv)	
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$ginv()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	        m <- solve(data, ...)
	        x$inv(m)
	        m
}
