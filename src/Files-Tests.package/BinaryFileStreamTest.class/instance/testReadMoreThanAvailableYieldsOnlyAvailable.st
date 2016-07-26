testing
testReadMoreThanAvailableYieldsOnlyAvailable
	
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	self assert: (file next:2) equals: #[].
	
	"then we put one element and we close it"
	file nextPut: 1.
	file close.
	
	file := self fileStreamForFileNamed: 'testFile'.
	self assert: (file next:2) equals: #[1].