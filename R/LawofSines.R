LawofSines <- function(sides, angles, findAngle = TRUE){
    if (findAngle) { # find missing angle
        a <- sides[1]; b <- sides[2]
        A <- angles[1] 
        return(asin(b*sin(A*pi/180)/a)*180/pi)
    } else { # find missing side length
        a <- sides[1] 
        A <- angles[1]; B <- angles[2] 
        return(a*sin(B*pi/180)/sin(A*pi/180))
    }
}
