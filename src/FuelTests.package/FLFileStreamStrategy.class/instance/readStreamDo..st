reading
readStreamDo: aValuable
	"Evaluates the argument with a read stream. Answers the result."
	
	^self fileStreamClass
		oldFileNamed: self fileName
		do: [ :aStream | aValuable value: aStream binary ]