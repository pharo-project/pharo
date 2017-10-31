testing
testNext
	| testString filename  |
	testString := 'éèàôüößäóñíá'.
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
			self assert: (stream peek = $é ).
			self assert: (stream next = $é ).
			self assert: (stream next = $è ).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 