tests
testCreateDirectoryNoParent
	| path |
	path := Path * 'griffle' / 'nurp'.
	self 
		should: [filesystem createDirectory: path]
		raise: DirectoryDoesNotExist.
	
	
