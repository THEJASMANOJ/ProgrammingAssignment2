
## 2 functions are used

##library(MASS) is used to calculate inverse of matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
 set<-function(y){
                x<<-y
                inv<--NULL
                  }
get<-function()x
setinv<-function(inverse)inv<<-inverse
getinv<--function(){
                    inver<-ginv(x)
                    inver%*%x
                     }
list(set=set,get=get,
     setinv=setinv,
     getinv=getinv)
 }


##

cacheSolve <- function(x, ...) 
  {
  inv<-x$getinv()
  if(!is.null(inv)){
                    message("getting the cached data!!")
                    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv ##RETURNS A MATRIX THAT IS INVERSE OF X
}
