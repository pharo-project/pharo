private
redirectNeededFor: uri actualFilename: actualFilename
	uri isDirectoryPath ifTrue: [ ^ false ].  
	^ (actualFilename endsWith: uri lastPathSegment) not