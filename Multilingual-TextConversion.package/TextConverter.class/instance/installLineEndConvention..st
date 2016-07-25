initialize-release
installLineEndConvention: lineEndStringOrNil

	
	lineEndStringOrNil ifNotNil:
		[latin1Encodings := latin1Encodings copy.
		latin1Encodings at: Character cr asciiValue + 1 put: (self convertFromSystemString: lineEndStringOrNil).
		latin1Map := ByteString nonAsciiMap copy.
		latin1Map at: Character cr asciiValue + 1 put: 1]