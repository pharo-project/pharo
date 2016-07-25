writing
store24BitBmpLine: pixelLine from: formBits startingAt: formBitsIndex width: width
	"Stores a single scanline containing 32bpp RGBA values in a 24bpp scanline. Swizzles the bytes as needed."
	<primitive: 'primitiveWrite24BmpLine' module:'BMPReadWriterPlugin'>

	| pixIndex rgb bitsIndex |	
	pixIndex := 0. "pre-increment"
	bitsIndex := formBitsIndex-1. "pre-increment"
	1 to: width do: [:j |
		rgb := (formBits at: (bitsIndex := bitsIndex+1)) bitAnd: 16rFFFFFF.
		pixelLine at: (pixIndex := pixIndex+1) put: (rgb bitAnd: 255).
		pixelLine at: (pixIndex := pixIndex+1) put: ((rgb bitShift: -8) bitAnd: 255).
		pixelLine at: (pixIndex := pixIndex+1) put: ((rgb bitShift: -16) bitAnd: 255).
	].
