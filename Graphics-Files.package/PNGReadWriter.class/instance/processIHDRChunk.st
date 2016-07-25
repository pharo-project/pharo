chunks
processIHDRChunk
	width := chunk 
		longAt: 1
		bigEndian: true.
	height := chunk 
		longAt: 5
		bigEndian: true.
	bitsPerChannel := chunk at: 9.
	colorType := chunk at: 10.
	"compression := chunk at: 11."	"TODO - validate compression"
	"filterMethod := chunk at: 12."	"TODO - validate filterMethod"
	interlaceMethod := chunk at: 13.	"TODO - validate interlace method"
	(#(2 4 6 ) includes: colorType) ifTrue: [ depth := 32 ].
	(#(0 3 ) includes: colorType) ifTrue: 
		[ depth := bitsPerChannel min: 8.
		colorType = 0 ifTrue: 
			[ "grayscale"
			palette := self grayColorsFor: depth ] ].
	bitsPerPixel := (BPP at: colorType + 1) at: bitsPerChannel highBit.
	bytesPerScanline := (width * bitsPerPixel + 7) // 8.
	rowSize := width * depth + 31 >> 5