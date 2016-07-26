huffman encoding
decodeBlockInto: anArray component: aColorComponent dcTable: huffmanDC acTable: huffmanAC 
	| byte i zeroCount |
	byte := stream decodeValueFrom: huffmanDC.
	byte ~= 0 ifTrue: 
		[ byte := self 
			scaleAndSignExtend: (self getBits: byte)
			inFieldWidth: byte ].
	byte := aColorComponent updateDCValue: byte.
	anArray atAllPut: 0.
	anArray 
		at: 1
		put: byte.
	i := 2.
	[ i <= DCTSize2 ] whileTrue: 
		[ byte := stream decodeValueFrom: huffmanAC.
		zeroCount := byte >> 4.
		byte := byte bitAnd: 15.
		byte ~= 0 
			ifTrue: 
				[ i := i + zeroCount.
				byte := self 
					scaleAndSignExtend: (self getBits: byte)
					inFieldWidth: byte.
				anArray 
					at: (JPEGNaturalOrder at: i)
					put: byte ]
			ifFalse: 
				[ zeroCount = 15 
					ifTrue: [ i := i + zeroCount ]
					ifFalse: [ ^ self ] ].
		i := i + 1 ]