testing
isEmpty
	"Test if there are more data to read."

	^self isInBufferEmpty and: [self isDataAvailable not]