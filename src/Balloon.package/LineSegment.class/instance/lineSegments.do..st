vector functions
lineSegments: steps do: aBlock
	"Evaluate aBlock with the receiver's line segments"
	aBlock value: start value: end