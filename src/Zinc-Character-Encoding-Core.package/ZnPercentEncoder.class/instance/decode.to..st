converting
decode: stringStream to: byteStream
	| char |
	self decodePlusAsSpace.
	[ stringStream atEnd ]
		whileFalse: [ 
			((char := stringStream next) == $+ and: [ decodePlusAsSpace ])
				ifTrue: [ byteStream nextPut: 32 ]
				ifFalse: [ 
					char == $%
						ifTrue: [ byteStream nextPut: (self readHexFrom: stringStream) ]
						ifFalse: [ 
							char charCode < 128
								ifTrue: [ byteStream nextPut: char charCode ]
								ifFalse: [ self errorAsciiCharacterExpected ] ] ] ]