transforming
adhereTo: aRectangle
	"If the receiver lies outside aRectangle, return the nearest point on the boundary of the rectangle, otherwise return self."

	(aRectangle containsPoint: self) ifTrue: [^ self].
	^ ((x max: aRectangle left) min: aRectangle right)
		@ ((y max: aRectangle top) min: aRectangle bottom)