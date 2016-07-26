initialization
initialize
	super initialize.
	lastModFileDateTime := DateAndTime epoch.
	fileAttributeFormat := FaUnix.
	self versionMadeBy: 20. "Dos-compatible file attributes; ZIP v. 2.0. See comment for #versionMadeBy"
	self versionNeededToExtract: 20. "ZIP v. 2.0. See comment for #versionNeededToExtract"
	self bitFlag: 0.
	compressionMethod := CompressionStored.
	desiredCompressionMethod := CompressionDeflated.
	desiredCompressionLevel := CompressionLevelDefault.
	internalFileAttributes := 0.
	externalFileAttributes := 0.
	fileName := ''.
	cdExtraField := ''.
	localExtraField := ''.
	fileComment := ''.
	crc32 := 0.
	compressedSize := 0.
	uncompressedSize := 0.
	self unixFileAttributes: DefaultFilePermissions.