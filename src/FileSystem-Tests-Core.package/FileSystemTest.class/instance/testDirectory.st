tests
testDirectory
	| path |
	
	path := Path * 'plonk'.
	self markForCleanup: path.
	
	filesystem createDirectory: path.
	
	self assert: (filesystem exists: path).
	self assert: (filesystem isDirectory: path).
	
	filesystem delete: path.
	self deny: (filesystem isFile: path).
	self deny: (filesystem exists: path)