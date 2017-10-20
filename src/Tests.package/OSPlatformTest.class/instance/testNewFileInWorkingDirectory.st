testing
testNewFileInWorkingDirectory
	"This is a test to make sure new files are created in the current working directory"
	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	self assert: (FileSystem disk resolve: file name asPath parent) equals: OSPlatform current currentWorkingDirectoryPath asPath.
	file delete 