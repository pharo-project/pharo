tests
testCreateDirectoryExists
	| path |
	
	path := Path * 'griffle'.
	self markForCleanup: path.
	
	filesystem createDirectory: path.
	self 
		should: [filesystem createDirectory: path]
		raise: DirectoryExists.
		

	
	
