tests
testOpeningForAppendSetsPositionAtEnd

	| file |
	(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].
	file := (File named: 'asd.txt') openForAppend.
	self assert: file position equals: 3.