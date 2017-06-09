tests
testSlashInThePathname
	
	self should:[Path * 'home/test'] raise:Error.
	self should:[FileSystem root / 'home/test'] raise: Error.
	self shouldnt:[FileSystem root / 'home' / 'test'] raise:Error.
	self shouldnt:['/home/test' asFileReference] raise:Error.
	self should:[Path * 'home/test'] raise:Error.