installing
installFileNamed: aFileName
	self installStream: (FileStream readOnlyFileNamed: aFileName)