Extension { #name : #Color }

{ #category : #'*Graphics-Primitives' }
Color >> bitPatternForDepth: depth [ 
	"Return a Bitmap, possibly containing a stipple pattern, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps. The resulting Bitmap may be multiple words to represent a stipple pattern of several lines.  "
	"See also:	pixelValueAtDepth:	-- value for single pixel
				pixelWordAtDepth:	-- a 32-bit word filled with the pixel value"
	"Details: The pattern for the most recently requested depth is cached."
	"Note for depths > 2, there are stippled and non-stippled versions (generated with #balancedPatternForDepth: and #bitPatternForDepth:, respectively). The stippled versions don't work with the window bit caching of StandardSystemView, so we make sure that for these depths, only unstippled patterns are returned"
	(depth = cachedDepth and: [ depth <= 2 or: [ cachedBitPattern size = 1 ] ]) ifTrue: [ ^ cachedBitPattern ].
	( self isTransparent and: [ cachedBitPattern isNil] )
			 ifTrue: [ cachedBitPattern := Bitmap with: 0 . 
					^ cachedBitPattern ].
	cachedDepth := depth.
	depth > 2 ifTrue: [ ^ cachedBitPattern := Bitmap with: (self pixelWordForDepth: depth) ].
	depth = 1 ifTrue: [ ^ cachedBitPattern := self halfTonePattern1 ].
	depth = 2 ifTrue: [ ^ cachedBitPattern := self halfTonePattern2 ]
]

{ #category : #'*Graphics-Primitives' }
Color class >> cachedColormapFrom: sourceDepth to: destDepth [ 
	"Return a cached colormap for mapping between the given depths. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	| srcIndex map |
	CachedColormaps class == Array ifFalse: [ CachedColormaps := (1 to: 9) collect: [ :i | Array new: 32 ] ].
	srcIndex := sourceDepth.
	sourceDepth > 8 ifTrue: [ srcIndex := 9 ].
	(map := (CachedColormaps at: srcIndex) at: destDepth) ~~ nil ifTrue: [ ^ map ].
	map := self 
		computeColormapFrom: sourceDepth
		to: destDepth.
	(CachedColormaps at: srcIndex) 
		at: destDepth
		put: map.
	^ map
]

{ #category : #'*Graphics-Primitives' }
Color class >> colorMapIfNeededFrom: sourceDepth to: destDepth [
	"Return a colormap for mapping between the given depths, or nil if no colormap is needed."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!"

	sourceDepth = destDepth ifTrue: [^ nil].  "not needed if depths are the same"

	(sourceDepth >= 16) & (destDepth >= 16) ifTrue: [
		"mapping is done in BitBlt by zero-filling or truncating each color component"
		^ nil].

	^ self cachedColormapFrom: sourceDepth to: destDepth

]

{ #category : #'*Graphics-Primitives' }
Color class >> computeColorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix [

	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color.
		Assumed not to include subpixelAA"
		^ self computeIndexedColorConvertingMap: targetColor from: sourceDepth to: destDepth
	] ifFalse: [
		"source is 16-bit or 32-bit RGB.
		Might include subpixelAA"
		^ self computeRGBColorConvertingMap: targetColor to: destDepth keepSubPixelAA: keepSubPix
	]
]

{ #category : #'*Graphics-Primitives' }
Color class >> computeColormapFrom: sourceDepth to: destDepth [ 
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
]

{ #category : #'*Graphics-Primitives' }
Color class >> computeIndexedColorConvertingMap: targetColor from: sourceDepth to: destDepth [ 
	| map |
	map := (IndexedColors 
		copyFrom: 1
		to: (1 bitShift: sourceDepth)) collect: 
		[ :cc | | f c | 
		f := 1.0 - ((cc red + cc green + cc blue) / 3.0).
		c := targetColor notNil 
			ifTrue: 
				[ destDepth = 32 
					ifTrue: [ targetColor * f alpha: f ]
					ifFalse: 
						[ targetColor 
							alphaMixed: f * 1.5
							with: self white ] ]
			ifFalse: [ cc ].
		destDepth = 32 
			ifTrue: [ c pixelValueForDepth: destDepth ]
			ifFalse: 
				[ f = 0.0 
					ifTrue: [ 0 ]
					ifFalse: [ c pixelValueForDepth: destDepth ] ] ].
	map := map as: Bitmap.
	^ map
]

{ #category : #'*Graphics-Primitives' }
Color class >> computeRGBColorConvertingMap: targetColor to: destDepth keepSubPixelAA: keepSubPix [ 
	"Builds a colormap intended to convert from subpixelAA black values to targetColor values.
	keepSubPix
		ifTrue: [ Answer colors that also include subpixelAA ]
		ifFalse: [ 
			Take fullpixel luminance level. Apply it to targetColor.
			I.e. answer colors with NO subpixelAA ]"
	| mask map c bitsPerColor r g b f v |
	destDepth > 8 
		ifTrue: [ bitsPerColor := 5	"retain maximum color resolution" ]
		ifFalse: [ bitsPerColor := 4 ].
	"Usually a bit less is enough, but make it configurable"
	bitsPerColor := bitsPerColor min: self aaFontsColormapDepth.
	mask := (1 bitShift: bitsPerColor) - 1.
	map := Bitmap new: (1 bitShift: 3 * bitsPerColor).
	0 
		to: map size - 1
		do: 
			[ :i | 
			r := (i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask.
			g := (i bitShift: 0 - bitsPerColor) bitAnd: mask.
			b := (i bitShift: 0) bitAnd: mask.
			f := 1.0 - ((r + g + b) / 3.0 / mask).
			c := targetColor notNil 
				ifTrue: 
					[ (keepSubPix and: [ destDepth > 8 ]) 
						ifTrue: 
							[ self 
								r: (1.0 - (r / mask)) * targetColor red
								g: (1.0 - (g / mask)) * targetColor green
								b: (1.0 - (b / mask)) * targetColor blue
								alpha: f * targetColor alpha	"alpha will be ignored below, in #pixelValueForDepth: if destDepth ~= 32" ]
						ifFalse: 
							[ destDepth = 32 
								ifTrue: [ targetColor * f alpha: f * targetColor alpha ]
								ifFalse: 
									[ targetColor 
										alphaMixed: f * 1.5
										with: self white ] ] ]
				ifFalse: 
					[ self  
						r: r
						g: g
						b: b
						range: mask ].	"This is currently used only to keep some SubPixelAA on destDepth = 8, using a single pass of rule 25"
			v := destDepth = 32 
				ifTrue: [ c pixelValueForDepth: destDepth ]
				ifFalse: 
					[ f < 0.1 
						ifTrue: [ 0 ]
						ifFalse: [ c pixelValueForDepth: destDepth ] ].
			map 
				at: i + 1
				put: v ].
	^ map
]

{ #category : #'*Graphics-Primitives' }
Color class >> computeRGBColormapFor: destDepth bitsPerColor: bitsPerColor [ 
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
]

{ #category : #'*Graphics-Primitives' }
Color >> halfTonePattern1 [
	"Return a halftone-pattern to approximate luminance levels on 1-bit deep Forms."
	| lum |
	lum := self luminance.
	lum < 0.1 ifTrue: [ ^ Bitmap with: 4294967295 ].	"black"
	lum < 0.4 ifTrue: 
		[ ^ Bitmap 
			with: 3149642683
			with: 4008636142 ].	"dark gray"
	lum < 0.6 ifTrue: 
		[ ^ Bitmap 
			with: 1431655765
			with: 2863311530 ].	"medium gray"
	lum < 0.9 ifTrue: 
		[ ^ Bitmap 
			with: 1145324612
			with: 286331153 ].	"light gray"
	^ Bitmap with: 0	"1-bit white"
]

{ #category : #'*Graphics-Primitives' }
Color >> halfTonePattern2 [
	"Return a halftone-pattern to approximate luminance levels on 2-bit deep Forms."
	| lum |
	lum := self luminance.
	lum < 0.125 ifTrue: [ ^ Bitmap with: 1431655765 ].	"black"
	lum < 0.25 ifTrue: 
		[ ^ Bitmap 
			with: 1431655765
			with: 3722304989 ].	"1/8 gray"
	lum < 0.375 ifTrue: 
		[ ^ Bitmap 
			with: 3722304989
			with: 2004318071 ].	"2/8 gray"
	lum < 0.5 ifTrue: 
		[ ^ Bitmap 
			with: 4294967295
			with: 2004318071 ].	"3/8 gray"
	lum < 0.625 ifTrue: [ ^ Bitmap with: 4294967295 ].	"4/8 gray"
	lum < 0.75 ifTrue: 
		[ ^ Bitmap 
			with: 4294967295
			with: 3149642683 ].	"5/8 gray"
	lum < 0.875 ifTrue: 
		[ ^ Bitmap 
			with: 4008636142
			with: 3149642683 ].	"6/8 gray"
	lum < 1.0 ifTrue: 
		[ ^ Bitmap 
			with: 2863311530
			with: 3149642683 ].	"7/8 gray"
	^ Bitmap with: 2863311530	"opaque white"

	"handy expression for computing patterns for 2x2 tiles;
 set p to a string of 4 letters (e.g., 'wggw' for a gray-and-
 white checkerboard) and print the result of evaluating:
| p d w1 w2 |
p := 'wggw'.
d := Dictionary new.
d at: $b put: '01'.
d at: $w put: '10'.
d at: $g put: '11'.
w1 := (d at: (p at: 1)), (d at: (p at: 2)).
w1 := '2r', w1, w1, w1, w1, w1, w1, w1, w1, ' hex'.
w2 := (d at: (p at: 3)), (d at: (p at: 4)).
w2 := '2r', w2, w2, w2, w2, w2, w2, w2, w2, ' hex'.
Array with: (Compiler evaluate: w1) with: (Compiler evaluate: w2) 
"
]

{ #category : #'*Graphics-Primitives' }
Color class >> maskingMap: depth [ 
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero. Cache the most recently used map."
	| sizeNeeded |
	depth <= 8 
		ifTrue: [ sizeNeeded := 1 bitShift: depth ]
		ifFalse: [ sizeNeeded := 4096 ].
	(MaskingMap == nil or: [ MaskingMap size ~= sizeNeeded ]) ifTrue: 
		[ MaskingMap := Bitmap 
			new: sizeNeeded
			withAll: 4294967295.
		MaskingMap 
			at: 1
			put: 0	"transparent" ].
	^ MaskingMap
]

{ #category : #'*Graphics-Primitives' }
Color class >> pixelScreenForDepth: depth [ 
	"Return a 50% stipple containing alternating pixels of all-zeros and all-ones to be used as a mask at the given depth."
	| mask bits |
	mask := (1 bitShift: depth) - 1.
	bits := 2 * depth.
	[ bits >= 32 ] whileFalse: 
		[ mask := mask bitOr: (mask bitShift: bits).	"double the length of mask"
		bits := bits + bits ].
	^ Bitmap 
		with: mask
		with: mask bitInvert32
]
