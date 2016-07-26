tests
testIsFile
	self assert: self entry isFile.
	self deny: self entry isDirectory