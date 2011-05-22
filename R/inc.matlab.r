repmat <- function(A,n,m) {
  if( length(dim(A)) == 1)
    d = c(length(A) * n , m)
  else
    d = dim(A) * c(n,m)
  return( array(A ,dim = d)) 
}

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


# spreads an array
spread <- function(A,c) {
  
}

