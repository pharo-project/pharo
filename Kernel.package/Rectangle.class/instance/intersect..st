rectangle functions
intersect: aRectangle 
	"Answer a Rectangle that is the area in which the receiver overlaps with 
	aRectangle."
	^ self intersect: aRectangle ifNone:[0@0 extent:0@0]