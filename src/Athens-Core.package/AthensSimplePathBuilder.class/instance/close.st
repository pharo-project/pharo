path commands
close
	"close the current contour"
	self addSegment: (AthensCloseSegment new point: contourStartPt).

	contourStartPt := nil.
	open := false