testing
testNextPutAllStartingAt
	| testString filename  |
	testString := 'éèàôüößäóñíá'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				next: 3 putAll: testString startingAt: 3;
				crlf].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream nextMatchAll: 'àôü' ).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 