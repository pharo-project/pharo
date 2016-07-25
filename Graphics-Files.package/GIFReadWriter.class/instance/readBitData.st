private-decoding
readBitData
	"using modified Lempel-Ziv Welch algorithm."
	| outCodes outCount bitMask initCodeSize code curCode oldCode inCode finChar i bytes f c packedBits hasLocalColor localColorSize maxOutCodes |
	maxOutCodes := 4096.
	offset := self readWord @ self readWord.	"Image Left@Image Top"
	width := self readWord.
	height := self readWord.

	"---
	Local Color Table Flag        1 Bit
	Interlace Flag                1 Bit
	Sort Flag                     1 Bit
	Reserved                      2 Bits
	Size of Local Color Table     3 Bits
	----"
	packedBits := self next.
	interlace := (packedBits bitAnd: 64) ~= 0.
	hasLocalColor := (packedBits bitAnd: 128) ~= 0.
	localColorSize := 1 bitShift: (packedBits bitAnd: 7) + 1.
	hasLocalColor ifTrue: [ localColorTable := self readColorTable: localColorSize ].
	pass := 0.
	xpos := 0.
	ypos := 0.
	rowByteSize := (width + 3) // 4 * 4.
	remainBitCount := 0.
	bufByte := 0.
	bufStream := ByteArray new readStream.
	outCodes := ByteArray new: maxOutCodes + 1.
	outCount := 0.
	prefixTable := Array new: 4096.
	suffixTable := Array new: 4096.
	initCodeSize := self next.
	bitMask := (1 bitShift: initCodeSize) - 1.
	self setParameters: initCodeSize.
	bitsPerPixel > 8 ifTrue: [ ^ self error: 'never heard of a GIF that deep' ].
	bytes := ByteArray new: rowByteSize * height.
	[ (code := self readCode) = eoiCode ] whileFalse: 
		[ code = clearCode 
			ifTrue: 
				[ self setParameters: initCodeSize.
				curCode := oldCode := code := self readCode.
				finChar := curCode bitAnd: bitMask.
				"Horrible hack to avoid running off the end of the bitmap.  Seems to cure problem reading some gifs!?"
				xpos = 0 
					ifTrue: 
						[ ypos < height ifTrue: 
							[ bytes 
								at: ypos * rowByteSize + xpos + 1
								put: finChar ] ]
					ifFalse: 
						[ bytes 
							at: ypos * rowByteSize + xpos + 1
							put: finChar ].
				self updatePixelPosition ]
			ifFalse: 
				[ curCode := inCode := code.
				curCode >= freeCode ifTrue: 
					[ curCode := oldCode.
					outCodes 
						at: (outCount := outCount + 1)
						put: finChar ].
				[ curCode > bitMask ] whileTrue: 
					[ outCount > maxOutCodes ifTrue: [ ^ self error: 'corrupt GIF file (OutCount)' ].
					outCodes 
						at: (outCount := outCount + 1)
						put: (suffixTable at: curCode + 1).
					curCode := prefixTable at: curCode + 1 ].
				finChar := curCode bitAnd: bitMask.
				outCodes 
					at: (outCount := outCount + 1)
					put: finChar.
				i := outCount.
				[ i > 0 ] whileTrue: 
					[ "self writePixel: (outCodes at: i) to: bits"
					bytes 
						at: ypos * rowByteSize + xpos + 1
						put: (outCodes at: i).
					self updatePixelPosition.
					i := i - 1 ].
				outCount := 0.
				prefixTable 
					at: freeCode + 1
					put: oldCode.
				suffixTable 
					at: freeCode + 1
					put: finChar.
				oldCode := inCode.
				freeCode := freeCode + 1.
				self checkCodeSize ] ].
	prefixTable := suffixTable := nil.
	f := ColorForm 
		extent: width @ height
		depth: 8.
	f bits copyFromByteArray: bytes.
	"Squeak can handle depths 1, 2, 4, and 8"
	bitsPerPixel > 4 ifTrue: [ ^ f ].
	"reduce depth to save space"
	c := ColorForm 
		extent: width @ height
		depth: (bitsPerPixel = 3 
				ifTrue: [ 4 ]
				ifFalse: [ bitsPerPixel ]).
	f displayOn: c.
	^ c