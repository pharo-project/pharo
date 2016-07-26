path primitives
rename: oldFileFullName to: newFileFullName 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name 
	or if there is an existing file with the new name."

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil