printing
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: '->'.
	substitutions printElementsOn: aStream