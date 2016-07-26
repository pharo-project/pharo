printing
printOn: aStream

	aStream
		nextPutAll: 'ManifestBuilder of ';
		nextPutAll: manifestClass printString.