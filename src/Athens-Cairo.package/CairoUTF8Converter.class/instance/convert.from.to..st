converting
convert: aString from: start to: end

	self checkBufferSpace: 4*(end-start + 2).
	bs reset.
	
	encoder next: end-start+1 
		putAll: aString 
		startingAt: start 
		toStream: bs .
		
	"DO NOT NULL-Terminate because its not used"

	^ bs position.