streams
redirectStdin
	"Creates a default stream and set it as stdin. 
	This method can be called directly by the user to note that he wants to map that standard stream,
	or by our own if createMissingStandardStreams is true."
	
	stdinStream := self createADefaultReadStream 