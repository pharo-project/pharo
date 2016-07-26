tests
testEntries

	| ref entries |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	
	ref := filesystem  / 'alpha'.
	entries := ref entries.
	self assert: entries size = 2.
	entries do:
		[:entry |
		self assert: entry class = FileSystemDirectoryEntry.
		self assert: (entry reference isChildOf: ref).
		self assert: (#('beta' 'gamma') includes: entry reference basename)]