testing
testUpToEnd
	| testString filename  |
	testString := 'élève en Français'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: testString .
		].
	
	filename asFileReference
		readStreamDo: [ :stream | 
			| str1 |
			self assert: ( stream peek = $é ).
			self assert: ( (stream next: 5) = 'élève' ).
			str1 := stream upToEnd .
			self assert: ( str1 = ' en Français') .
		] 