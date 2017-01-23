accessing
nextTo: aName in: aDirectory
	
	^ String streamContents: [ :stream |
		stream
			nextPutAll: (aName copyUpToLast: $.);
			nextPut: $.;
			nextPutAll: self nextSuffix ]