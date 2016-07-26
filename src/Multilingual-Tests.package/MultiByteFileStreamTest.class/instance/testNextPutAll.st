testing
testNextPutAll
	| testString filename  |
	testString := '123abcd'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: '123';
				nextPutAll: 'abcd';
				crlf].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream nextMatchAll:  testString).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 