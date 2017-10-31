testing
testNextPut
	| filename  |
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPut: $1;
				nextPut: $a;
				nextPut: $2;
				nextPut: $b ;
				crlf].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream nextMatchAll:  '1a2b').
		] ]
	ensure: [ filename asFileReference ensureDelete ] 