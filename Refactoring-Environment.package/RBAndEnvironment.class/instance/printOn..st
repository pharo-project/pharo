printing
printOn: aStream 
	environment printOn: aStream.
	aStream nextPutAll: ' & '.
	andedEnvironment printOn: aStream.