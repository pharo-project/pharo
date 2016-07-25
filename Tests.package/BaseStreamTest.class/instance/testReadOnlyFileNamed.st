testing
testReadOnlyFileNamed
	| testString fileName file |
	"Given"
	fileName := self testFileName.
	testString := 'testString'.
	"When"
	[self baseStreamType
		fileNamed: fileName
		do: [:streamType | streamType nextPut: testString].
	"Then"
	file := self baseStreamType readOnlyFileNamed: fileName.
	self assert: file next = testString.
	file close]
		ensure: [ fileName asFileReference delete ]