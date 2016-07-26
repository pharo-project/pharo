accessing
byteAt: byteAddress put: byte 
	"Insert a byte into a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:put:.  See Form>>#pixelAt:put:"
	| longWord shift lowBits longAddr |
	(byte < 0 or: [ byte > 255 ]) ifTrue: [ ^ self errorImproperStore ].
	lowBits := byteAddress - 1 bitAnd: 3.
	longWord := self at: (longAddr := (byteAddress - 1 - lowBits) // 4 + 1).
	shift := (3 - lowBits) * 8.
	longWord := longWord - (longWord bitAnd: (255 bitShift: shift)) + (byte bitShift: shift).
	self 
		at: longAddr
		put: longWord.
	^ byte