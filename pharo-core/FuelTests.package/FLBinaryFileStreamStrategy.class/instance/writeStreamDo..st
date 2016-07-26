writing
writeStreamDo: aValuable
	"Evaluates the argument with a write stream. Answers the result."

	^ ((Smalltalk globals at: #File) named: self fileName)writeStreamDo: [ :aStream | 
		aValuable value: aStream binary ]