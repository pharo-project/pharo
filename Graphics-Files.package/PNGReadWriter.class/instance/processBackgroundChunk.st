chunks
processBackgroundChunk
	"Transcript show: '  BACKGROUND: ',chunk printString."
	| val red green blue max |
	colorType = 3 ifTrue: 
		[ backColor := palette at: chunk first + 1.
		^ self ].
	max := (2 raisedTo: bitsPerChannel) - 1.
	(colorType = 0 or: [ colorType = 4 ]) ifTrue: 
		[ val := chunk 
			unsignedShortAt: 1
			bigEndian: true.
		backColor := Color gray: val / max.
		^ self ].
	(colorType = 2 or: [ colorType = 6 ]) ifTrue: 
		[ red := chunk 
			unsignedShortAt: 1
			bigEndian: true.
		green := chunk 
			unsignedShortAt: 3
			bigEndian: true.
		blue := chunk 
			unsignedShortAt: 5
			bigEndian: true.
		backColor := Color 
			r: red / max
			g: green / max
			b: blue / max.
		^ self ]
	"self halt."

	"====
The bKGD chunk specifies a default background color to present the image against. Note that viewers are not bound to honor this chunk; a viewer can choose to use a different background. 

For color type 3 (indexed color), the bKGD chunk contains: 


   Palette index:  1 byte

The value is the palette index of the color to be used as background. 

For color types 0 and 4 (grayscale, with or without alpha), bKGD contains: 


   Gray:  2 bytes, range 0 .. (2^bitdepth)-1

(For consistency, 2 bytes are used regardless of the image bit depth.) The value is the gray level to be used as background. 

For color types 2 and 6 (truecolor, with or without alpha), bKGD contains: 


   Red:   2 bytes, range 0 .. (2^bitdepth)-1
   Green: 2 bytes, range 0 .. (2^bitdepth)-1
   Blue:  2 bytes, range 0 .. (2^bitdepth)-1

(For consistency, 2 bytes per sample are used regardless of the image bit depth.) This is the RGB color to be used as background. 

When present, the bKGD chunk must precede the first IDAT chunk, and must follow the PLTE chunk, if any. 
==="