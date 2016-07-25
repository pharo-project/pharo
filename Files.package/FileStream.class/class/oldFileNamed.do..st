instance creation
oldFileNamed: fileName do: aBlock
	"Returns the result of aBlock."
	
	^ self detectFile: [ self oldFileNamed: fileName ] do: aBlock