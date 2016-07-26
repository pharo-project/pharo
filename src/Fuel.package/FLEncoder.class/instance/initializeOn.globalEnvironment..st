initialize-release
initializeOn: aStream globalEnvironment: aDictionary
	
	self initialize.
	stream := FLBufferedWriteStream on: aStream.
	globalEnvironment := aDictionary.