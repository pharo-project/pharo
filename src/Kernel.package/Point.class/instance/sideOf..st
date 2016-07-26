geometry
sideOf: otherPoint 
	"Returns #left, #right or #center if the otherPoint lies to the left, right or on the line given by the vector from 0@0 to self"
	| side |
	side := (self crossProduct: otherPoint) sign.
	^ { #right . #center . #left } at: side + 2