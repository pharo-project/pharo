tests
testChildFileReturnsFile

	| dir childName child childFile |
	dir := Directory named: 'testtesttest'.
	dir create.
	
	childName := dir name, Directory pathDelimiter asString, 'childFile'.
	child := File named: childName. 
	child writeStream close.

	childFile := dir childNamed: 'childFile'.
	self assert: childFile exists.
	self assert: childFile isFile.
	self deny: childFile isDirectory.
	childFile delete.
	dir delete.