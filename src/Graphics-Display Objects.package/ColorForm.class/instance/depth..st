private
depth: bitsPerPixel

	bitsPerPixel > 8 ifTrue: [self error: 'ColorForms only support depths up to 8 bits'].
	super depth: bitsPerPixel.
