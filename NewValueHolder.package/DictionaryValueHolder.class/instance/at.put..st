protocol
at: key put: anObject

	value at: key put: anObject.
	self valueChanged: anObject.
	^ anObject