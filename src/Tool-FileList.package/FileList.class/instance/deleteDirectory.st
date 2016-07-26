own services
deleteDirectory
	self basicDeleteDirectory.
	self updateFileList.
	self updateDirectory.
