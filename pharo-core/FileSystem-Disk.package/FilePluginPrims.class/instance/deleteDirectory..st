path primitives
deleteDirectory: fullPath
	"Delete the directory named by the given path. 
	Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed
