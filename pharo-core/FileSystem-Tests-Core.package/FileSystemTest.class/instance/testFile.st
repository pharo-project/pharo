tests
testFile
	| path |
	
	path := Path * 'gooly'.
	self markForCleanup: path.
	
	(filesystem open: path writable: true) writeStream close.
	self assert: (filesystem exists: path).
	self deny: (filesystem isDirectory: path).
	self assert: (filesystem isFile: path).
	
	filesystem delete: path.
	self deny: (filesystem exists: path)