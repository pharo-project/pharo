tests
testWithExistingFile

	self createFileWith: 'existing'.

	name := strategy nextTo: 'existing' in: self directory.
	
	self assert: name isString.
	self deny: name = 'existing'.
	self deny: (self fileReferenceWith: name) exists