testing
testNextMatchAll
	| testString filename  |
	testString := 'é è à ô ü ö ß ä ó ñ í á'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: testString;
				crlf ].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream nextMatchAll:  testString).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 