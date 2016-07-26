support
createFile: aPath
	filesystem ensureCreateDirectory: aPath parent.
	(filesystem writeStreamOn: aPath) close