testing
testFileNamed
	| testString fileName |
	"Given"
	fileName := self testFileName.
	testString := 'testString'.
	"When"
	[self baseStreamType
		fileNamed: fileName
		do: [:streamType | streamType nextPut: testString].
	"Then"
	self baseStreamType
		fileNamed: fileName
		do: [:streamType | self assert: streamType next = testString]]
		ensure: [ (FileSystem disk workingDirectory / fileName) ensureDelete ].