chunks
processTransparencyChunk

	"Transcript show: '  TRANSPARENCY ',chunk printString."
	colorType = 0
		ifTrue:
			[transparentPixelValue := chunk unsignedShortAt: 1 bigEndian: true.
			bitsPerChannel <= 8
				ifTrue: [palette at: transparentPixelValue + 1 put: Color transparent]
				ifFalse: [palette at: 1 put: Color transparent].
			^self].
	colorType = 2
		ifTrue:
			[| red green blue |
			red :=  chunk unsignedShortAt: 1 bigEndian: true.
			green :=  chunk unsignedShortAt: 3 bigEndian: true.
			blue :=  chunk unsignedShortAt: 5 bigEndian: true.
			transparentPixelValue := bitsPerChannel <= 8
				ifTrue: [16rFF00 + red << 8 + green << 8 + blue]
				ifFalse: [red << 16 + green << 16 + blue].
			^self].
	colorType = 3
		ifTrue:
			[chunk withIndexDo: [ :alpha :index |
				palette at: index put: ((palette at: index) alpha: alpha/255)].
			^self].
