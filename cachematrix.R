#Input a matrix into makeCacheMatrix, like I have below
#It will store the four functions (set, get, setinv, getinv)

makeCacheMatrix<-function(x=matrix()) {
    i<-NULL
    set<-function(y) 
        {x<<-y
        i<<-NULL}
    get<-function() 
        {x}
    setinv<-function(solve) 
        {i<<-solve}
    getinv<-function() 
        {i}
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

#In cacheSolve, the first thing it does is look for the
#pre-existing solved matrix, which if run through makeCacheMatrix
#previously, will exist and pull up with the getinv function
#This is what the if statement is checking for.

#If it does exist, it will produce the comment 'getting cached 
#data', then give you the inverse matrix. Otherwise, it runs
#through the solve function to produce the inverse for the first
#time, using the setinv function.

cacheSolve <- function(x, ...) {
    i<-x$getinv()
    if(!is.null(i))
        {message('getting cached data')
        return(i)}
    data<-x$get()
    i<-solve(data, ...)
    x$setinv(i)
    i
}

#Example: if you run it through twice, you'll see the 'getting
#cached data' comment, but otherwise the same output.

x<-matrix(c(4,7,2,6),2,2)

x1<-makeCacheMatrix(x)

cacheSolve(x1)









