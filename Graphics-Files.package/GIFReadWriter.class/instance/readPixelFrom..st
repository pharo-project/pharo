private-encoding
readPixelFrom: bits 
	"Since bits is a Bitmap with 32 bit values, watch out for the
padding at the end of each row.  But, GIF format already wants padding to
32 bit boundary!  OK as is.  tk 9/14/97"
	| pixel |
	ypos >= height ifTrue: [ ^ nil ].
	pixel := bits byteAt: ypos * rowByteSize + xpos + 1.
	self updatePixelPosition.
	^ pixel