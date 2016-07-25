stream in
nextAllInBuffer
	"Return all data currently in the inBuffer,"

	^self nextInBuffer: inNextToWrite - lastRead - 1