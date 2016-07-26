initialization
initialize
	super initialize.
	
	self contentsBrick: self newContentsBrick.
	self arrowBrick: self newArrowBrick.
	self previewBrick: self newPreviewBrick