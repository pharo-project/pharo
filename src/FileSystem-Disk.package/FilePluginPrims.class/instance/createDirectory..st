path primitives
createDirectory: fullPath
	"Create a directory named by the given path. 
	Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	^ nil
