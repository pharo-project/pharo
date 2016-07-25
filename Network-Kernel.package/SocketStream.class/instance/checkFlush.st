private
checkFlush
	"If autoFlush is true we flush if
	we have reached the bufferSize
	of data in the outBuffer."

	(autoFlush and: [outNextToWrite > bufferSize])
		ifTrue: [self flush]