initialize-release
initialize
	super initialize.
	self contentType: self mimeTypeWithBoundary  
	