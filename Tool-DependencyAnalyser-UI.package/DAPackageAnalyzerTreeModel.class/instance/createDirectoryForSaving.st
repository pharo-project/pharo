saving
createDirectoryForSaving
	| dir |
	dir := FileSystem workingDirectory / self defaultDirectory.
	dir ensureCreateDirectory