rectangle functions
encompass: aPoint 
	"Answer a Rectangle that contains both the receiver and aPoint."

	^ Rectangle 
		origin: (origin min: aPoint)
		corner: (corner max:  aPoint)