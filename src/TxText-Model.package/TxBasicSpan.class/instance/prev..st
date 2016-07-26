accessing
prev: anObject
	
	self basicPrev: anObject.
	anObject basicNext: self.
	^ anObject