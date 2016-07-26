private-decoding
readHeader
	| is89 byte hasColorMap |
	(self hasMagicNumber: 'GIF87a' asByteArray) 
		ifTrue: [ is89 := false ]
		ifFalse: 
			[ (self hasMagicNumber: 'GIF89a' asByteArray) 
				ifTrue: [ is89 := true ]
				ifFalse: [ ^ self error: 'This does not appear to be a GIF file' ] ].
	self readWord.	"skip Screen Width"
	self readWord.	"skip Screen Height"
	byte := self next.
	hasColorMap := (byte bitAnd: 128) ~= 0.
	bitsPerPixel := (byte bitAnd: 7) + 1.
	byte := self next.	"skip background color."
	self next ~= 0 ifTrue: 
		[ is89 ifFalse: [ ^ self error: 'corrupt GIF file (screen descriptor)' ] ].
	hasColorMap 
		ifTrue: [ colorPalette := self readColorTable: (1 bitShift: bitsPerPixel) ]
		ifFalse: 
			[ "Transcript cr; show: 'GIF file does not have a color map.'."
			colorPalette := nil	"Palette monochromeDefault" ]