writing
writeStreamDo: aValuable
	"Evaluates the argument with a write stream. Answers the result."

	^self fileStreamClass
		forceNewFileNamed: self fileName 
		do: [ :aStream | aValuable value: aStream binary ]