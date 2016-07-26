adding
addVertex: aPoint
	"Add a vertex to the path."

	self vertices add: aPoint.
	self basicBounds ifNotNil: [
		self bounds: (self bounds quickMergePoint: aPoint)] 