tests
testCopyAndDelete
	"Check that FileSystem>>copyAndDelete:to: works within a filesystem.
	DiskFileSystemTest>>testMoveMemoryToDisk checks that #copyAndDelete:to: works across filesystems."
	| folder testString f1 f1s f2 |
	
	folder := filesystem workingDirectory.
	testString := 'To be copied and deleted'.

	f1 := folder / 'f1'.
	f1s := f1 writeStream.
	[ f1s nextPutAll: testString ] ensure: [ f1s close ].
	f2 := folder / 'f2'.
	
	"Cleanup after running"
	self 
		markForCleanup: f1;
		markForCleanup: f2.	
	
	filesystem copyAndDelete: f1 to: f2.
	self deny: f1 exists.
	self assert: f2 exists.
	self assert: f2 contents equals: testString.