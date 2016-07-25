stream in
nextAvailable
	"Answer all the data currently available,
	in buffer or in socket."

	self isInBufferEmpty ifFalse: [^self nextAllInBuffer].
	self isDataAvailable ifTrue: [self receiveData].
	^self nextAllInBuffer