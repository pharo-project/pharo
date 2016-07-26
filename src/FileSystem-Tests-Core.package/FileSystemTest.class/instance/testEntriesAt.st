tests
testEntriesAt
	| directory entries |
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self 
		markForCleanup: directory / 'griffle';
		markForCleanup: directory / 'bint';
		markForCleanup: directory.
	
	entries := filesystem entriesAt: directory.
	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: FileSystemDirectoryEntry).
		self assert: ea reference parent path = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea reference basename).
		self assert: ea isDirectory ]