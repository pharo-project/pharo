instance creation
fileNamed: fileName do: aBlock
	"Returns the result of aBlock."
	
	^ self detectFile: [ self fileNamed: fileName ] do: aBlock