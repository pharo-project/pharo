instance creation
readFrom: stream usingType: mimeType andLength: length
	"Create and return a new streaming entity of mimeType and length from stream.
	Clients can/must use the embedded ZnLimitedReadStream to read all content, 
	or call #consumeContent when they are done. They should not close the stream."
	
	^ (self type: mimeType length: length)
		readFrom: stream;
		yourself
