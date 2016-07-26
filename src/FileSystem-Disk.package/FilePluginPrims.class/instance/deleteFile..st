path primitives
deleteFile: aFileName
	"Delete the file of the given name. 
	Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil
