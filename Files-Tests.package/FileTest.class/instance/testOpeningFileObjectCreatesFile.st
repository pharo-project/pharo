tests
testOpeningFileObjectCreatesFile

	(File named: 'asd.txt') writeStream.
	"Our File implementation should not cache anything.
	This test is based on this assumption."
	self assert: (File named: 'asd.txt') exists.