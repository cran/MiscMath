LawofCosines <-
function(sides){
    a <- sides[1]; b <- sides[2]; c <- sides[3]
    acos(-(c^2 - a^2 - b^2)/(2*a*b))*180/pi
}
