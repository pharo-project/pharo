accessing
depth: bitsPerPixel
	(bitsPerPixel > 32 or:
		[(bitsPerPixel bitAnd: bitsPerPixel-1) ~= 0])
		ifTrue: [self error: 'bitsPerPixel must be 1, 2, 4, 8, 16 or 32'].
	depth := bitsPerPixel