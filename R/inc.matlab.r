repmat <- function(A,n,m) {
  if( length(dim(A)) == 1)
    d = c(length(A) * n , m)
  else
    d = dim(A) * c(n,m)
  return( array(A ,dim = d)) 
}


# spread the array A[1,2,3] to 
# what dim specifies in the order given
# spread(A , c(2) , 10) will insert a dimension
# in the second index of size 10
spread <- function(A, loc, dims) {
  adims = dim(A)
  
  # check dimensions
  l = length(loc)
  if (max(loc)> length(dim(A))+l) { 
    stop('incorrect dimensions in spread')
  }

  # total dimension not in order
  sdim = c(dim(A),dims)

  # permutation of dims
  edim = c()
  oi =1        # original dims
  ni =length(dim(A))+1 # new dims
  for (i in c(1: (length(dim(A))+l))) {
    if (i %in% loc) {
      edim = c(edim,ni)
      ni = ni+1
    } else {
      edim = c(edim,oi)
      oi = oi +1
    }
  }

  return( aperm( array(A,dim=sdim),edim)) 
}

##tic <- function(reset=FALSE) {
#  .env <- environment()
#
#  if (reset) {
#    assign('last_tick',  proc.time()[3], envir = .env)
#    return(0)
#  } else {
#    return( proc.time()[3] - get('last_tick',envir = .env))
#  }
#}


library(rgl)
mesh <- function(x=(1:nrow(Z)) / nrow(Z) ,y=(1:nrow(Z)) / nrow(Z),Z) {
	
	z <-  (Z - min(min(Z))) / (max(max(Z)) - min(min(Z))) 
	
	zlim <- range(z)
	zlen <- zlim[2] - zlim[1] + 1
	
	#colorlut <- heat.colors(zlen,alpha=0) # height color lookup table
	
	#col <- colorlut[ Z-zlim[1]+1 ] # assign colors to heights for each point
	open3d()
	persp3d(x,y,Z,col = 'blue', size=5,box=FALSE)
	#rgl.surface(x,y,z)
}


