public - string reading
sourceCodeAt: sourcePointer 
	"Retrieves the source code for the pointer received as argument"
	
	^ self
		readStreamAt: sourcePointer 
		ifPresent: [ :stream | stream nextChunk ]
		ifAbsent: [ '' ]