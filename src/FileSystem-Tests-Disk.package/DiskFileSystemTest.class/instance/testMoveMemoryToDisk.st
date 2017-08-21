tests
testMoveMemoryToDisk
	"Test moving a file from the memory filesystem to the disk filesystem.
	This ensures that the copyAndDelete:to: is called correctly."
	| memfs out testString old new |
	[
		memfs := FileSystem memory.
		old := memfs / 'testMoveMemoryToDisk_old'.
		out := old writeStream.
		testString := 'To be moved to disk'.
		[ out nextPutAll: testString ] ensure: [ out close ].
		
		new := FileLocator imageDirectory / 'testMoveMemoryToDisk_new'.
		old moveTo: new.
		
		self deny: (memfs / 'testMoveMemoryToDisk_old') exists.
		self assert: new exists.
		self assert: new contents equals: testString.
	] ensure: [ 
		old ensureDelete.
		new ensureDelete.
	]