accessing
byteAt: byteAddress 
	"Extract a byte from a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:.  See Form>>#pixelAt:"
	| lowBits |
	lowBits := byteAddress - 1 bitAnd: 3.
	^ ((self at: (byteAddress - 1 - lowBits) // 4 + 1) bitShift: (lowBits - 3) * 8) bitAnd: 255