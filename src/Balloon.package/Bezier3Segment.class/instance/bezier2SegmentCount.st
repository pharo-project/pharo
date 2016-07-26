private
bezier2SegmentCount
	"Compute the number of quadratic bezier segments needed to approximate
	this cubic with less than a 1-pixel error"
	^ self bezier2SegmentCount: 1.0