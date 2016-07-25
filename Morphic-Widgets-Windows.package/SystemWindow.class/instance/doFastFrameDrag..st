events
doFastFrameDrag: grabPoint
	"Do fast frame dragging from the given point"

	(FastDraggingFrameMorph forDragging: self clickedAt: grabPoint) openInWorld
