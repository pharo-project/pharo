testing
testPeekFor
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
			self deny: (stream peekFor: $a). "If peekFor fails, it doesn't advance position"
			self assert: (stream peekFor: $à)
		] ]
	ensure: [ filename asFileReference ensureDelete ] 