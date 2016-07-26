testing
isAsciiString

	^ self allSatisfy: [ :each | each asciiValue <= 127 ]
