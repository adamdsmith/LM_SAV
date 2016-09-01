nearestPointOnSegment = function(s, p){
  # Adapted from http://pastebin.com/n9rUuGRh
  ap = c(p[1] - s[1,1], p[2] - s[1,2])
  ab = c(s[2,1] - s[1,1], s[2,2] - s[1,2])
  t = sum(ap*ab) / sum(ab*ab)
  t = ifelse(t<0,0,ifelse(t>1,1,t))
  x = s[1,1] + ab[1] * t 
  y = s[1,2] + ab[2] * t
  c(x, y, (x-p[1])^2 + (y-p[2])^2)  # Return nearest point and distance
}

nearestPointOnLine = function(coordsLine, coordsPoints){
  nearest_points = vapply(2:nrow(coordsLine), 
                          function(x) 
                            nearestPointOnSegment(coordsLine[(x-1):x,], coordsPoints),
                          FUN.VALUE=c(0,0,0))
  
  # Return coordinates of the nearest point in this line  
  nearest_points[1:2, which.min(nearest_points[3,])]  
}

snapPointsToLines <- function( points, lines, maxDist = NA, keepID = FALSE) {
  
  pacman::p_load(rgeos)
  
  if (!is.na(maxDist)) {
    w = gWithinDistance(points, lines, dist=maxDist, byid=TRUE)
    validPoints = apply(w,2,any)
    validLines = apply(w,1,any)
    points = points[validPoints,]
    lines =  lines[validLines,]
  }
  
  d = gDistance(points, lines, byid=TRUE) 
  nearest_line_index = apply(d, 2, which.min) # Position of each nearest line in lines object 
  
  coordsLines = coordinates(lines)  
  coordsPoints = coordinates(points)  
  
  # Get coordinates of nearest points lying on nearest lines
  mNewCoords = vapply(1:length(points), 
                      function(x) 
                        nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]], 
                                           coordsPoints[x,]), FUN.VALUE=c(0,0))
  
  # Recover lines' Ids (Ids and index differ if maxDist is given)
  if (!is.na(maxDist)) nearest_line_id = as.numeric(rownames(d)[nearest_line_index])+1 
  else nearest_line_id = nearest_line_index 
  
  # Create data frame and sp points
  if (keepID) df = cbind(points@data, nearest_line_id) 
  else df = points@data
  
  SpatialPointsDataFrame(coords=t(mNewCoords), data=df, 
                         proj4string=CRS(proj4string(points)),
                         match.ID = FALSE)
}


