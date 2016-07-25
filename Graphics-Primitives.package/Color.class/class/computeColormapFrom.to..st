colormaps
computeColormapFrom: sourceDepth to: destDepth 
	"Compute a colorMap for translating between the given depths. A colormap is a Bitmap whose entries contain the pixel values for the destination depth. Typical clients use cachedColormapFrom:to: instead."
	| map bitsPerColor |
	sourceDepth < 16 
		ifTrue: 
			[ "source is 1-, 2-, 4-, or 8-bit indexed color"
			map := (IndexedColors 
				copyFrom: 1
				to: (1 bitShift: sourceDepth)) collect: [ :c | c pixelValueForDepth: destDepth ].
			map := map as: Bitmap ]
		ifFalse: 
			[ "source is 16-bit or 32-bit RGB"
			destDepth > 8 
				ifTrue: [ bitsPerColor := 5	"retain maximum color resolution" ]
				ifFalse: [ bitsPerColor := 4 ].
			map := self 
				computeRGBColormapFor: destDepth
				bitsPerColor: bitsPerColor ].

	"Note: zero is transparent except when source depth is one-bit deep"
	sourceDepth > 1 ifTrue: 
		[ map 
			at: 1
			put: 0 ].
	^ map