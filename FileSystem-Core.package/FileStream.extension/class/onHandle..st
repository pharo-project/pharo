*FileSystem-Core
onHandle: aFileSystemHandle
	^ self concreteStream new
		open: aFileSystemHandle fullName
		forWrite: aFileSystemHandle isWritable