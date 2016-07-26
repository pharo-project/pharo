private
hackBits: bitThing
	"This method provides an initialization so that BitBlt may be used, eg, to 
	copy ByteArrays and other non-pointer objects efficiently.
	The resulting form looks 4 wide, 8 deep, and bitThing-size-in-words high."
	width := 4.
	depth := 8.
	bitThing class isBits ifFalse: [self error: 'bitThing must be a non-pointer object'].
	bitThing class isBytes
		ifTrue: [height := bitThing basicSize // 4]
		ifFalse: [height := bitThing basicSize].
	bits := bitThing