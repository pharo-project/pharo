resize/collapse
doFastWindowReframe: aSideOrCornerName
	"Do fast frame resizing  from the given corner or side"

	(FastDraggingFrameMorph forResizing: self fromLocation: aSideOrCornerName) openInWorld