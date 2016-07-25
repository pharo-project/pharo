accessing
filterFileNames: aCollection forVersionNamed: aString
	^ aCollection select: [ :ea | ea name = aString ]