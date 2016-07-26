instance creation
newFileNamed: fileName do: aBlock
	"Returns the result of aBlock."
	
	^ self detectFile: [ self newFileNamed: fileName ] do: aBlock