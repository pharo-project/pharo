instance creation
fromPoint: aPoint
	"Create a margin whose values are based on the point value: top and bottom are y and left right are x."
	
	^ self new fromPoint: aPoint; yourself