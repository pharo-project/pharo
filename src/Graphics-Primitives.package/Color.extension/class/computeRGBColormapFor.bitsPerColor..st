*Graphics-Primitives
computeRGBColormapFor: destDepth bitsPerColor: bitsPerColor 
	"Compute a colorMap for translating from 16-bit or 32-bit RGB color to the given depth, using the given number of of bits per color component."
	| mask map c |
	(#(3 4 5 ) includes: bitsPerColor) ifFalse: 
		[ self error: 'BitBlt only supports 3, 4, or 5 bits per color component' ].
	mask := (1 bitShift: bitsPerColor) - 1.
	map := Bitmap new: (1 bitShift: 3 * bitsPerColor).
	0 
		to: map size - 1
		do: 
			[ :i | 
			c := self 
				r: ((i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask)
				g: ((i bitShift: 0 - bitsPerColor) bitAnd: mask)
				b: ((i bitShift: 0) bitAnd: mask)
				range: mask.
			map 
				at: i + 1
				put: (c pixelValueForDepth: destDepth) ].
	map 
		at: 1
		put: (self transparent pixelWordForDepth: destDepth).	"zero always transparent"
	^ map