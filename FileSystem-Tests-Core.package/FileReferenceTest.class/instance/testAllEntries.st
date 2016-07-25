tests
testAllEntries
	| ref entries |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	
	ref := filesystem / 'alpha'.
	entries := ref allEntries.
	self assert: entries size = 4.
	entries do:
		[:entry |
		self assert: entry class = FileSystemDirectoryEntry.
		self assert: (ref = entry reference or: [ref contains: entry reference])].
	self assert: (entries collect: [:ea | ea basename]) = #('alpha' 'beta' 'gamma' 'delta')