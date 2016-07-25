instance creation
reference: ref creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBoolean
	"Create a directory entry for the file reference ref, with the creation time, cTime, the modification time, mTime. aBoolean indicates if the entry represents a directory or a file of size given by anInteger"
	
	^ self basicNew
		initializeWithRef: ref
		creation: cTime
		modification: mTime
		isDir: aBoolean
		size: anInteger
		posixPermissions: posixNumber
		isSymlink: symlinkBoolean