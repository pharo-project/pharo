comparing
min: aPoint 
	"Answer the upper left corner of the rectangle uniquely defined by the receiver and the argument, aPoint."

	^ (x min: aPoint x) @ (y min: aPoint y)