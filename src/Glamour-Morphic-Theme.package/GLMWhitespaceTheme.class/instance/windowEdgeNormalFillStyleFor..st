fill-styles
windowEdgeNormalFillStyleFor: anEdgeGrip
	"Return the normal window edge fillStyle for the given edge grip."
	self flag: 'this is used for the long edges'.
	^ super windowEdgeNormalFillStyleFor: anEdgeGrip
	"self glamorousDarkBaseColorFor: anEdgeGrip"