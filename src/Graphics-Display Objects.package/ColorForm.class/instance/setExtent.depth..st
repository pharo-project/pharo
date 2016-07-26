private
setExtent: extent depth: bitsPerPixel
	"Create a virtual bit map with the given extent and bitsPerPixel."

	bitsPerPixel > 8 ifTrue: [self error: 'ColorForms only support depths up to 8 bits'].
	super setExtent: extent depth: bitsPerPixel.
