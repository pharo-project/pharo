tests
testWithNotExistingFile

	name := strategy nextTo: 'inexistent' in: self directory.
	
	self assert: name isString.
	self deny: (self fileReferenceWith: name) exists