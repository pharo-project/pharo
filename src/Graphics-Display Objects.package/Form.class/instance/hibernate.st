filein/out
hibernate
	"Replace my bitmap with a compactly encoded representation (a ByteArray).  It is vital that BitBlt and any other access to the bitmap (such as writing to a file) not be used when in this state.  Since BitBlt will fail if the bitmap size is wrong (not = bitsSize), we do not allow replacement by a byteArray of the same (or larger) size."

	"NOTE: This method copies code from Bitmap compressToByteArray so that it can
	nil out the old bits during the copy, thus avoiding 2x need for extra storage."
	| compactBits lastByte |
	(bits isMemberOf: Bitmap) ifFalse: [^ self  "already hibernated or weird state"].
	compactBits := ByteArray new: (bits size*4) + 7 + (bits size//1984*3).
	lastByte := bits compress: bits toByteArray: compactBits.
	lastByte < (bits size*4) ifTrue:
		[bits := nil.  "Let GC reclaim the old bits before the copy if necessary"
		bits := compactBits copyFrom: 1 to: lastByte]