instance creation
newBounds: bounds color: color

	^ (self new privateBounds: bounds) privateColor: color
