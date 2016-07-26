accessing
indexOf: anInteger startingAt: start

	(anInteger isInteger and: [ anInteger between: 0 and: 255 ]) ifFalse: [ ^0 ].
	^ByteString indexOfAscii: anInteger inString: self startingAt: start