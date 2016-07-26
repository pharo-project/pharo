geometry
to: end sideOf: otherPoint 
	"Returns #left, #right, #center if the otherPoint lies to the left, right or on the line given by the vector from self to end"
	^ end - self sideOf: otherPoint - self