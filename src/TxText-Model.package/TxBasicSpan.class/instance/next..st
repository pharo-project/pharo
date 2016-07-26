accessing
next: anObject
	
	self basicNext: anObject.
	anObject basicPrev: self.
	^ anObject