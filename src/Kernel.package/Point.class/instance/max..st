comparing
max: aPoint 
	"Answer the lower right corner of the rectangle uniquely defined by the receiver and the argument, aPoint."

	^ (x max: aPoint x) @ (y max: aPoint y)