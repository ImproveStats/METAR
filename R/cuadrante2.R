cuadrante2 <-
function(punto)
{
		x <- punto[1];y<- punto[2]
		angulo <- abs(atan(y/x)*180/pi)
		limite <- 10
		if(x>=0 & y>=0) out <- ifelse(angulo<limite,4,3)
		if(x<0 & y>=0) out <- ifelse(angulo<limite,2,3)
		if(x<0 & y<0) out <- ifelse(angulo<limite,2,1)
		if(x>=0 & y<0) out <- ifelse(angulo<limite,4,1)
		return(out)
}
