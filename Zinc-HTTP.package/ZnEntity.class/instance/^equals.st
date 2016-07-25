comparing
= other
	self class = other class ifFalse: [ ^ false ].
	^ self contentType = other contentType and: [ self contentLength = other contentLength ]