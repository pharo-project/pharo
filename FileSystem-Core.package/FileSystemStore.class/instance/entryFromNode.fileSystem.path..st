private
entryFromNode: aNode fileSystem: aFilesystem path: aPath
	^ FileSystemDirectoryEntry
		fileSystem: aFilesystem
		path: aPath
		creation: (self basicCreationTime: aNode)
		modification:(self basicModificationTime: aNode)
		isDir: (self basicIsDirectory: aNode)
		size: (self basicSize: aNode)
		posixPermissions: (self basicPosixPermissions: aNode)
		isSymlink: (self basicIsSymlink: aNode)