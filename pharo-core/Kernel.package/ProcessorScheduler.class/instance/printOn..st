printing
printOn: aStream
	self isSelfEvaluating ifFalse: [^super printOn: aStream].
	aStream nextPutAll: #Processor