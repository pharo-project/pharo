regression test
regressionTest "ZipWriteStream regressionTest"
	"Compress and decompress everything we can 
	find to validate that compression works as expected."
	self regressionTestFrom: FileSystem workingDirectory.