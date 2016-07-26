instance creation
fileSystem: aFilesystem path: aPath creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBooleam
	"Create a directory entry given a filesystem and a path in such filesystem. In addition, the creation and modification time are required as well as a boolean that indicates whether the entry is a folder or a file and its size."

	^ self 
		reference: (aFilesystem referenceTo: aPath)
		creation: cTime
		modification: mTime
		isDir: aBoolean
		size: anInteger
		posixPermissions: posixNumber
		isSymlink: symlinkBooleam