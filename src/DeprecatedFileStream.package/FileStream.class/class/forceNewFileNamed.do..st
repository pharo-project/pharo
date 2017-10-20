instance creation
forceNewFileNamed: fileName do: aBlock
	"Returns the result of aBlock."
	
	^ self detectFile: [ self forceNewFileNamed: fileName ] do: aBlock