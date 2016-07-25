testing
testReset
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
			self assert: (stream peekFor: $é ).
			self assert: (stream peekFor: $è ).
			stream reset.
			self assert: (stream peekFor: $é ).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 