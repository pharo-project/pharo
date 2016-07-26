encoding
encodeBitmap: aBitmap 
	"Append to the receiver an Integer as the next two bytes."
	aBitmap writeOn: stream.
