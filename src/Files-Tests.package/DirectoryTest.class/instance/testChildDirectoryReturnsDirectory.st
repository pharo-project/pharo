tests
testChildDirectoryReturnsDirectory

	| dir childName child childDir |
	dir := Directory named: 'testtesttest'.
	dir create.
	
	childName := dir name, Directory pathDelimiter asString, 'childDir'.
	child := Directory named: childName. 
	child create.

	childDir := dir childNamed: 'childDir'.
	self assert: childDir exists.
	self assert: childDir isDirectory.
	self deny: childDir isFile.
	childDir delete.
	dir delete.