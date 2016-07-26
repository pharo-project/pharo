printing
printOn: aStream
	modifier printOn: aStream.
	aStream << ' + '.
	character printOn: aStream.