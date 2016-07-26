initialization
initialize
	super initialize.
	externalFileAttributes := self mapPermissionsFromUnix: DefaultDirectoryPermissions.
	super desiredCompressionMethod: CompressionStored.