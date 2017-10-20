preparation
exportFileSystemInStFile

	self
		exportPackages: #( 'FileSystem-Core' 'FileSystem-Disk' )
		usingInitializeScript: '
FileLocator initialize.
DiskStore initialize.
FileHandle initialize.
MultiByteFileStream initialize.'
		intoFile: 'FileSystem.st'