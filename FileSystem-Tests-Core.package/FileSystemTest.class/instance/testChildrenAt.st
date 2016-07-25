tests
testChildrenAt
	| directory entries |
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self markForCleanup: directory / 'griffle'.
	self markForCleanup: directory / 'bint'.
	self markForCleanup: directory.
	
	entries := filesystem childrenAt: directory.
	
	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: Path).
		self assert: ea parent = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea basename) ]