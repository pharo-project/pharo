testing
testCurrentWorkingDirectory
	"This is a test to make sure getCurrentWorkingDirectory is working"

 self assert: FileSystem workingDirectory fullName equals:  OSPlatform current currentWorkingDirectoryPath.
 self assert: 'relativePath' asFileReference asAbsolute parent fullName equals: OSPlatform current currentWorkingDirectoryPath