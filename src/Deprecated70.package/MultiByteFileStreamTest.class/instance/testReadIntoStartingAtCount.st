testing
testReadIntoStartingAtCount
	| testString filename buffer |
	testString := 'élève en Français'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	filename asFileReference ensureDelete.
	filename asFileReference
		writeStreamDo: [ :stream | 
			stream
				nextPutAll: testString;
				crlf ].
	buffer := String new: testString size.
	[ 
	filename asFileReference
		readStreamDo: [ :stream | 
			self assert: (stream readInto: buffer startingAt: 1 count: testString size) equals: testString size.
			self assert: buffer equals: testString ] ]
		ensure: [ filename asFileReference ensureDelete ]