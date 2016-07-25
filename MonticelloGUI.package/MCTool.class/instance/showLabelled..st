opening
showLabelled: labelString
	
	modal := false.
	self label: labelString.
	self window openInWorld.
	self setDefaultFocus.
	^ self window.