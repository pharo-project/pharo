interface
loadAllFileNames
	^ self clientDo:
		[:client |
		self parseDirectoryListing: client getDirectory]