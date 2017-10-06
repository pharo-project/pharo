instance creation
readOnlyFileNamed: fileName do: aBlock
	"Open the existing file with the given name for read-only access and pass it as argument to aBlock.
	Returns the result of aBlock."
	
	^ self detectFile: [ self readOnlyFileNamed: fileName ] do: aBlock