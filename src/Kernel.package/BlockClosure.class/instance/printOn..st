printing
printOn: aStream
	OpalCompiler isActive 
		ifTrue: [ aStream nextPutAll:  self sourceNode formattedCode ]
		ifFalse: [ aStream nextPutAll: '[...]']