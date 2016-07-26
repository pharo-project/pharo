reading
read24BmpFile
	"Read 24-bit pixel data from the given a BMP stream."
	| form formBits pixelLine bitsIndex bitBlt |
	form := Form 
		extent: biWidth @ biHeight
		depth: 32.
	pixelLine := ByteArray new: (24 * biWidth + 31) // 32 * 4.
	bitsIndex := (form height - 1) * biWidth + 1.
	formBits := form bits.
	1 
		to: biHeight
		do: 
			[ :i | 
			pixelLine := stream nextInto: pixelLine.
			self 
				read24BmpLine: pixelLine
				into: formBits
				startingAt: bitsIndex
				width: biWidth.
			bitsIndex := bitsIndex - biWidth ].
	bitBlt := BitBlt toForm: form.
	bitBlt combinationRule: 7.	"bitOr:with:"
	bitBlt halftoneForm: (Bitmap with: 4278190080).
	bitBlt copyBits.
	^ form