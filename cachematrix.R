##The function "makeCacheMatrix" solve the inverse of a square numeric 
##or complex matrix and store it in an environment that is different from the 
##current environment. The function "cacheSolve" tests if the (posterior) matrix is solved
##and if is not, apply the function "solve" to get the inverse   

##This function solve the matrix and store it

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



##This function work with the "makeCacheMatrix" function and
##tests if the inverse matrix its solved if its not, the matrix
##its solved

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
