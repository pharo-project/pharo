tests
testOpeningFileNormallyDoesNotDeleteExistingFile

	| file |
	file := (File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].

	file := (File named: 'asd.txt') readStream.
	self assert: file size equals: 3.