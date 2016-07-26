reading
readStreamDo: aValuable
	"Evaluates the argument with a read stream. Answers the result."
	
	^ ((Smalltalk globals at: #File) named: self fileName) readStreamDo: [ :aStream |
		aValuable value: aStream binary ]