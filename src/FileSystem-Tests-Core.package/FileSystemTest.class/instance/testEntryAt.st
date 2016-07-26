tests
testEntryAt
	| path1 path2 entry1 entry2  |

	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	self markForCleanup: path2.
	
	filesystem createDirectory: path1.
	(Delay forSeconds: 2) wait. "#creationTime seems limited to 1 second resolution"
	filesystem createDirectory: path2.
	
	entry1 := filesystem entryAt: path1.
	entry2 := filesystem entryAt: path2.
	
	self assert: entry1 isDirectory.
	self assert: entry2 isDirectory.
	self assert: entry1 reference = (filesystem referenceTo: path1) asAbsolute.
	self assert: entry2 reference = (filesystem referenceTo: path2) asAbsolute.

	self assert: entry1 creationTime < entry2 creationTime.
	self assert: entry1 modificationTime < entry2 modificationTime.
