initialize
setExtent: extent depth: bitsPerPixel
	bits ifNotNil: [self error: 'bits are already set'].
	bitsPerPixel == 32 ifFalse: [self error: 'only 32 bits for now'.  "see 'rowPitch' below"].
	width := extent x asInteger.
	width < 0 ifTrue: [width := 0].
	height := extent y asInteger.
	height < 0 ifTrue: [height := 0].
	depth := bitsPerPixel.
	bits := self 
		primCreateManualSurfaceWidth: width 
		height: height 
		rowPitch: width * 4
		depth: bitsPerPixel
		isMSB: true.