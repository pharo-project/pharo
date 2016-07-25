initialization
initialize
	super initialize.
	externalFileName := ''.
	compressionMethod := CompressionStored.
	self mapPermissionsFromUnix: DefaultFilePermissions.
