vector functions
lineSegmentsDo: aBlock
	"Evaluate aBlock with the receiver's line segments"
	aBlock value: start value: end