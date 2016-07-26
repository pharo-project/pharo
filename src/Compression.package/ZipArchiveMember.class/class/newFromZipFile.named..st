instance creation
newFromZipFile: stream named: fileName
	^ZipFileMember newFrom: stream named: fileName