makeCacheMatrix <- function(x = matrix()) {
la<-NULL
  set<-function(y){
  x<<-y
  la<<-NULL
}
gettera2<-function() x
settera2<-function(solve) la<<- solve #it is inverse matrix for calculating
gettera2<-function() la
list(settera=settera, gettera=gettera,
   settera2=settera2,
   gettera2=gettera2)
}

cacheSolve <- function(x, ...) {
     la<-x$gettera2()
    if(!is.null(la)){
      message("it is inverse matrix cached")
      return(la)
    }
    mat<-x$get()
    la<-solve(mat, ...)
    x$settera2(la)
    la
}
