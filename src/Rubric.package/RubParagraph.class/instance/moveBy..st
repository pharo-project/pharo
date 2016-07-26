geometry updating
moveBy: aPoint
	container := container translateBy: aPoint.
	self composer moveBy: aPoint.
	textArea recomputeSelection.
	textArea invalidRect: (self position extent: self extent)
