primitives
primHashBlock: blockBitmap using: workingTotalsBitmap
	"Hash the given block (a Bitmap) of 80 32-bit words, using the given workingTotals."

	<primitive: 'primitiveHashBlock' module: 'DSAPrims'>
	^ self primitiveFailed
