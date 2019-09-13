"
I am PNGReadWriter.
I am a concrete subclass of ImageReadWriter.

I read and write the  Portable Network Graphics (PNG) image format.

  https://en.wikipedia.org/wiki/Portable_Network_Graphics

Submitted by Duane Maxwell
"
Class {
	#name : #PNGReadWriter,
	#superclass : #ImageReadWriter,
	#instVars : [
		'chunk',
		'form',
		'width',
		'height',
		'depth',
		'backColor',
		'bitsPerChannel',
		'colorType',
		'interlaceMethod',
		'bitsPerPixel',
		'bytesPerScanline',
		'thisScanline',
		'prevScanline',
		'rowSize',
		'idatChunkStream',
		'unknownChunks',
		'palette',
		'transparentPixelValue',
		'filtersSeen',
		'cachedDecoderMap',
		'bigEndian'
	],
	#classVars : [
		'BPP',
		'BlockHeight',
		'BlockWidth',
		'Debugging',
		'StandardColors',
		'StandardSwizzleMaps'
	],
	#category : #'Graphics-Files'
}

{ #category : #utilities }
PNGReadWriter class >> computeSwizzleMapForDepth: depth [
	"Answer a map that maps pixels in a word to their opposite location. Used for 'middle-endian' forms where the byte-order is different from the bit order (good joke, eh?)."
	| map swizzled |
	map := Bitmap new: 256.
	depth = 4 ifTrue:[
		0 to: 255 do:[:pix|
			swizzled := 0.
			swizzled := swizzled bitOr: (((pix bitShift: 0) bitAnd: 15) bitShift: 4).
			swizzled := swizzled bitOr: (((pix bitShift: -4) bitAnd: 15) bitShift: 0).
			map at: pix+1 put: swizzled.
		].
		^ColorMap colors: map
	].

	depth = 2 ifTrue:[
		0 to: 255 do:[:pix|
			swizzled := 0.
			swizzled := swizzled bitOr: (((pix bitShift: 0) bitAnd: 3) bitShift: 6).
			swizzled := swizzled bitOr: (((pix bitShift: -2) bitAnd: 3) bitShift: 4).
			swizzled := swizzled bitOr: (((pix bitShift: -4) bitAnd: 3) bitShift: 2).
			swizzled := swizzled bitOr: (((pix bitShift: -6) bitAnd: 3) bitShift: 0).
			map at: pix+1 put: swizzled.
		].
		^ColorMap colors: map
	].

	depth = 1 ifTrue:[
		0 to: 255 do:[:pix|
			swizzled := 0.
			swizzled := swizzled bitOr: (((pix bitShift: 0) bitAnd: 1) bitShift: 7).
			swizzled := swizzled bitOr: (((pix bitShift: -1) bitAnd: 1) bitShift: 6).
			swizzled := swizzled bitOr: (((pix bitShift: -2) bitAnd: 1) bitShift: 5).
			swizzled := swizzled bitOr: (((pix bitShift: -3) bitAnd: 1) bitShift: 4).
			swizzled := swizzled bitOr: (((pix bitShift: -4) bitAnd: 1) bitShift: 3).
			swizzled := swizzled bitOr: (((pix bitShift: -5) bitAnd: 1) bitShift: 2).
			swizzled := swizzled bitOr: (((pix bitShift: -6) bitAnd: 1) bitShift: 1).
			swizzled := swizzled bitOr: (((pix bitShift: -7) bitAnd: 1) bitShift: 0).
			map at: pix+1 put: swizzled.
		].
		^ColorMap colors: map
	].
	self error: 'Unrecognized depth'
]

{ #category : #initialize }
PNGReadWriter class >> debugging: aBoolean [ 
	Debugging := aBoolean
]

{ #category : #initialize }
PNGReadWriter class >> initialize [
	"
	PNGReadWriter initialize
	"
	BPP := { 
		#(1 2 4 8 16 ).
		#(0 0 0 0 0 ).
		#(0 0 0 24 48 ).
		#(1 2 4 8 0 ).
		#(0 0 0 16 32 ).
		#(0 0 0 0 0 ).
		#(0 0 0 32 64 ).
		#(0 0 0 0 0 )
	 }.
	BlockHeight := #(8 8 4 4 2 2 1 ).
	BlockWidth := #(8 4 4 2 2 1 1 ).
	StandardColors := Color indexedColors collect: 
		[ :aColor | 
		Color 
			r: (aColor red * 255) truncated / 255
			g: (aColor green * 255) truncated / 255
			b: (aColor blue * 255) truncated / 255 ].
	StandardSwizzleMaps := Array new: 4.
	#(1 2 4 ) do: 
		[ :i | 
		StandardSwizzleMaps 
			at: i
			put: (self computeSwizzleMapForDepth: i) ]
]

{ #category : #'image reading/writing' }
PNGReadWriter class >> typicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that I can read might commonly have"
	^#('png')
]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixels: y [ 
	"Handle non-interlaced pixels of supported colorTypes"
	| s |
	s := #(
		#copyPixelsGray:
		nil
		#copyPixelsRGB:
		#copyPixelsIndexed:
		#copyPixelsGrayAlpha:
		nil
		#copyPixelsRGBA:
	) at: colorType + 1.
	self 
		perform: s asSymbol
		with: y
]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixels: y at: startX by: incX [ 
	"Handle interlaced pixels of supported colorTypes"
	| s |
	s := #(
		#copyPixelsGray:at:by:
		nil
		#copyPixelsRGB:at:by:
		#copyPixelsIndexed:at:by:
		#copyPixelsGrayAlpha:at:by:
		nil
		#copyPixelsRGBA:at:by:
	) at: colorType + 1.
	self 
		perform: s asSymbol
		with: y
		with: startX
		with: incX
]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsGray: y [ 
	"Handle non-interlaced grayscale color mode (colorType = 0)"

	| base bits bytesLeft word |
	bitsPerChannel = 16 ifTrue: [
		"Warning: This is extremely slow. Besides we are downsampling to 8 bits!"
		| blitter |
		blitter := BitBlt bitPokerToForm: form.
		0 to: width - 1 do: [ :x |
			| high low value |
			high := thisScanline at: x * 2 + 1.
			low := thisScanline at: x * 2 + 2.
			value := (high * 256 + low = transparentPixelValue)
				ifTrue: [0 "transparent"]
				ifFalse: [high max: 1].
			blitter pixelAt: x @ y put: value ].
			^self ].

	"Just copy the bits"

	"This Smalltalk version might be easier to understand than the others below."
	base := y * (form width * bitsPerChannel + 31 // 32) + 1.
	bits := form bits.
	0 to: thisScanline size // 4 - 1 do: [ :i |
		| ii |
		ii := i * 4.
		"This somewhat weird mixture of (#* and #+) with (#bitShift: and #bitOr:) 
		is to make use of faster arithmetic bytecodes, but not of slow largeintegers."
		word :=
			(((thisScanline at: ii + 1) * 256 + 
			(thisScanline at: ii + 2) * 256 + 
			(thisScanline at: ii + 3)) bitShift: 8) bitOr: 
			(thisScanline at: ii + 4).
		bits at: base + i put: word ].	
	(bytesLeft := thisScanline size bitAnd: 3) = 0 ifFalse: [
		word := 0.
		thisScanline size - bytesLeft + 1 to: thisScanline size do: [ :ii |
			word := word * 256 + (thisScanline at: ii) ].
		word := word bitShift: 8 * (4 - bytesLeft).
		bits at: base + (thisScanline size // 4) put: word ].

	"This interesting technique (By Andreas Raab) is faster for very large images, but might be slower for small ones"
	"^self copyPixelsGrayWeirdBitBltHack: y ".
	"It uses the following method:
	PNGReadWriter >> copyPixelsGrayWeirdBitBltHack: y 
	""Handle non-interlaced black and white color mode (colorType = 0)
	By Andreas Raab""
	
	| source dest cmap |
	source := Form extent: 1 @ (thisScanline size // 4) depth: 32 bits: thisScanline.
	dest := Form extent: 1 @ (form bits size) depth: 32 bits: form bits.
	cmap := Smalltalk isLittleEndian
		ifTrue:[ColorMap 
					shifts: #(-24 -8 8 24) 
					masks: #(16rFF000000 16r00FF0000 16r0000FF00 16r000000FF)].
	(BitBlt toForm: dest)
		sourceForm: source;
		destX: 0 destY: (y * form width*bitsPerChannel//32) width: 1 height: (form width+31*bitsPerChannel//32);
		colorMap: cmap;
		combinationRule: 3;
		copyBits."
		
	"This interesting technique  (By Yoshiki Ohshima) is faster for very large images, but might be slower for small ones"
	"form bits copyFromByteArray2: thisScanline to: y * (form width* bitsPerChannel // 32)".
	"It uses the following method:
	BitMap >> copyFromByteArray2: byteArray to: i
	""This method should work with either byte orderings""

	| myHack byteHack |
	myHack := Form new hackBits: self.
	byteHack := Form new hackBits: byteArray.
	Smalltalk  isLittleEndian ifTrue: [byteHack swapEndianness].
	byteHack displayOn: myHack at:  0@i"
]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsGray: y at: startX by: incX [
	"Handle interlaced grayscale color mode (colorType = 0)"

	| offset bits blitter pixPerByte shifts b pixel mask pixelNumber |
	bitsPerChannel = 16
		ifTrue: [
			"Warning: This is extremely slow. Besides we are downsampling to 8 bits!"
			blitter := BitBlt bitPokerToForm: form.
			startX to: width-1 by: incX do: [ :x |
				| high low value |
				high := thisScanline at: x//incX<<1 + 1.
				low := thisScanline at: x//incX<<1 + 2.
				value := (high * 256 + low = transparentPixelValue)
					ifTrue: [0 "transparent"]
					ifFalse: [high max: 1].
				blitter pixelAt: x @ y put: value ].
				^self ].
	offset := y*rowSize+1.
	bits := form bits.
	bitsPerChannel = 8 ifTrue: [
		startX to: width-1 by: incX do: [ :x | | w |
			w := offset + (x>>2).
			b := 3- (x \\ 4) * 8.
			pixel := (thisScanline at: x // incX + 1)<<b.
			mask := (255<<b) bitInvert32.
			bits at: w put: (((bits at: w) bitAnd: mask) bitOr: pixel)
		].
		^ self
	].
	bitsPerChannel = 1 ifTrue: [
		pixPerByte := 8.
		mask := 1.
		shifts := #(7 6 5 4 3 2 1 0).
	].
	bitsPerChannel = 2 ifTrue: [
		pixPerByte := 4.
		mask := 3.
		shifts := #(6 4 2 0).
	].
	bitsPerChannel = 4 ifTrue: [
		pixPerByte := 2.
		mask := 15.
		shifts := #(4 0).
	].

	blitter := BitBlt bitPokerToForm: form.
	pixelNumber := 0.
	startX to: width-1 by: incX do: [ :x | | rawByte |
		rawByte := thisScanline at: (pixelNumber // pixPerByte) + 1.
		pixel := (rawByte >> (shifts at: (pixelNumber \\ pixPerByte) + 1)) bitAnd: mask.
		blitter pixelAt: (x@y) put: pixel.
		pixelNumber := pixelNumber + 1.
	].

]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsGrayAlpha: y [ 
	"Handle non-interlaced grayscale with alpha color mode (colorType = 4)"
	| i pixel gray b |
	b := BitBlt bitPokerToForm: form.
	bitsPerChannel = 8 
		ifTrue: 
			[ 0 
				to: width - 1
				do: 
					[ :x | 
					i := (x << 1) + 1.
					gray := thisScanline at: i.
					pixel := ((thisScanline at: i + 1) << 24) + (gray << 16) + (gray << 8) + gray.
					b 
						pixelAt: x @ y
						put: pixel ] ]
		ifFalse: 
			[ 0 
				to: width - 1
				do: 
					[ :x | 
					i := (x << 2) + 1.
					gray := thisScanline at: i.
					pixel := ((thisScanline at: i + 2) << 24) + (gray << 16) + (gray << 8) + gray.
					b 
						pixelAt: x @ y
						put: pixel ] ]
]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsGrayAlpha: y at: startX by: incX [ 
	"Handle interlaced grayscale with alpha color mode (colorType = 4)"
	| i pixel gray b |
	b := BitBlt bitPokerToForm: form.
	bitsPerChannel = 8 
		ifTrue: 
			[ startX 
				to: width - 1
				by: incX
				do: 
					[ :x | 
					i := (x // incX << 1) + 1.
					gray := thisScanline at: i.
					pixel := ((thisScanline at: i + 1) << 24) + (gray << 16) + (gray << 8) + gray.
					b 
						pixelAt: x @ y
						put: pixel ] ]
		ifFalse: 
			[ startX 
				to: width - 1
				by: incX
				do: 
					[ :x | 
					i := (x // incX << 2) + 1.
					gray := thisScanline at: i.
					pixel := ((thisScanline at: i + 2) << 24) + (gray << 16) + (gray << 8) + gray.
					b 
						pixelAt: x @ y
						put: pixel ] ]
]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsIndexed: y [
	"Handle non-interlaced indexed color mode (colorType = 3)"
	| hack hackBlt swizzleHack swizzleBlt scanline hackDepth |
	scanline := ByteArray new: bytesPerScanline + 3 // 4 * 4.
	scanline replaceFrom: 1 to: thisScanline size with: thisScanline startingAt: 1.
	hackDepth := bigEndian ifTrue:[form depth] ifFalse:[form depth negated].
	hack := Form extent: width@1 depth: hackDepth bits: scanline.
	hackBlt := BitBlt toForm: form.
	hackBlt sourceForm: hack.
	hackBlt combinationRule: Form over.
	hackBlt destOrigin: 0@y.
	hackBlt width: width; height: 1.

	(form depth < 8 and:[bigEndian not]) ifTrue:[
		swizzleHack := Form new hackBits: scanline.
		swizzleBlt := BitBlt toForm: swizzleHack.
		swizzleBlt sourceForm: swizzleHack.
		swizzleBlt combinationRule: Form over.
		swizzleBlt colorMap: (StandardSwizzleMaps at: form depth).
		swizzleBlt copyBits.
	].

	hackBlt copyBits.
]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsIndexed: y at: startX by: incX [ 
	"Handle interlaced indexed color mode (colorType = 3)"
	| offset bits pixPerByte shifts blitter pixel mask pixelNumber |
	offset := y * rowSize + 1.
	bits := form bits.
	bitsPerChannel = 8 ifTrue: 
		[ startX 
			to: width - 1
			by: incX
			do: 
				[ :x | | w b | 
				w := offset + (x >> 2).
				b := (3 - (x \\ 4)) * 8.
				pixel := (thisScanline at: x // incX + 1) << b.
				mask := (255 << b) bitInvert32.
				bits 
					at: w
					put: (((bits at: w) bitAnd: mask) bitOr: pixel) ].
		^ self ].
	bitsPerChannel = 1 ifTrue: 
		[ pixPerByte := 8.
		mask := 1.
		shifts := #(7 6 5 4 3 2 1 0 ) ].
	bitsPerChannel = 2 ifTrue: 
		[ pixPerByte := 4.
		mask := 3.
		shifts := #(6 4 2 0 ) ].
	bitsPerChannel = 4 ifTrue: 
		[ pixPerByte := 2.
		mask := 15.
		shifts := #(4 0 ) ].
	blitter := BitBlt bitPokerToForm: form.
	pixelNumber := 0.
	startX 
		to: width - 1
		by: incX
		do: 
			[ :x | | rawByte | 
			rawByte := thisScanline at: pixelNumber // pixPerByte + 1.
			pixel := rawByte >> (shifts at: pixelNumber \\ pixPerByte + 1) bitAnd: mask.
			blitter 
				pixelAt: x @ y
				put: pixel.
			pixelNumber := pixelNumber + 1 ]
]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsRGB: y [
	"Handle non-interlaced RGB color mode (colorType = 2)"

	| i pixel tempForm tempBits |
	(transparentPixelValue isNil and: [ bitsPerChannel = 8 ]) ifTrue: [ "Do the same trick as in #copyPixelsRGBA:"
		| targetIndex |
		tempBits := ByteArray new: thisScanline size * 4 // 3 withAll: 16rFF.
		tempForm := Form extent: width@1 depth: 32 bits: tempBits.
		targetIndex := 1.
		1 to: thisScanline size by: 3 do: [ :index |
			tempBits
				at: targetIndex put: (thisScanline at: index);
				at: targetIndex + 1 put: (thisScanline at: index + 1);
				at: targetIndex + 2 put: (thisScanline at: index + 2).
			targetIndex := targetIndex + 4 ].
		cachedDecoderMap 
			ifNil:[cachedDecoderMap := self rgbaDecoderMapForDepth: depth].
		(BitBlt toForm: form)
			sourceForm: tempForm;
			destOrigin: 0@y;
			combinationRule: Form over;
			colorMap: cachedDecoderMap;
			copyBits.
		^self ].
	tempForm := Form extent: width@1 depth: 32.
	tempBits := tempForm bits.
	pixel := LargePositiveInteger new: 4.
	pixel at: 4 put: 16rFF.
	bitsPerChannel = 8
		ifTrue:
			[i := 1.
			1 to: width do:
				[ :x |
				pixel
					at: 3 put: (thisScanline at: i);
					at: 2 put: (thisScanline at: i+1);
					at: 1 put: (thisScanline at: i+2).
				tempBits at: x put: pixel normalize.
				i := i + 3].
			transparentPixelValue
				ifNotNil:
					[1 to: width do: [ :x |
						(tempBits at: x) = transparentPixelValue
							ifTrue: [tempBits at: x put: 0]]]]
		ifFalse:
			[i := 1.
			1 to: width do:
				[ :x |
				(transparentPixelValue == nil or: [(1 to: 6) anySatisfy: [:k | (transparentPixelValue byteAt: k) ~= (thisScanline at: i + 6 - k)]])
					ifTrue:
						[pixel
							at: 3 put: (thisScanline at: i);
							at: 2 put: (thisScanline at: i+2);
							at: 1 put: (thisScanline at: i+4).
						tempBits at: x put: pixel normalize]
					ifFalse:
						[tempBits at: x put: 0].
				i := i + 6]].
	
	tempForm displayOn: form at: 0@y rule: Form over.

]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsRGB: y at: startX by: incX [
	"Handle interlaced RGB color mode (colorType = 2)"

	| i pixel tempForm tempBits xx loopsToDo |

	tempForm := Form extent: width@1 depth: 32.
	tempBits := tempForm bits.
	pixel := LargePositiveInteger new: 4.
	pixel at: 4 put: 16rFF.
	loopsToDo := width - startX + incX - 1 // incX.
	bitsPerChannel = 8
		ifTrue:
			[i := (startX // incX * 3) + 1.
			xx := startX+1.
			1 to: loopsToDo do:
				[ :j |
				pixel
					at: 3 put: (thisScanline at: i);
					at: 2 put: (thisScanline at: i+1);
					at: 1 put: (thisScanline at: i+2).
				tempBits at: xx put: pixel normalize.
				i := i + 3.
				xx := xx + incX].
			transparentPixelValue
				ifNotNil: [startX to: width-1 by: incX do: [ :x |
					(tempBits at: x+1) = transparentPixelValue
						ifTrue: [	tempBits at: x+1 put: 0]]]]
		ifFalse:
			[i := (startX // incX * 6) + 1.
			xx := startX+1.
			1 to: loopsToDo do:
				[ :j |
				(transparentPixelValue == nil or: [(1 to: 6) anySatisfy: [:k | (transparentPixelValue byteAt: k) ~= (thisScanline at: i + 6 - k)]])
					ifTrue:
						[pixel
							at: 3 put: (thisScanline at: i);
							at: 2 put: (thisScanline at: i+2);
							at: 1 put: (thisScanline at: i+4).
						tempBits at: xx put: pixel normalize.]
					ifFalse:
						[tempBits at: xx put: 0].
				i := i + 6.
				xx := xx + incX]].
	tempForm displayOn: form at: 0@y rule: Form over.


]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsRGBA: y [
	"Handle non-interlaced RGBA color modes (colorType = 6)"

	| i pixel tempForm tempBits ff |
	bitsPerChannel = 8 ifTrue: [
		ff := Form extent: width@1 depth: 32 bits: thisScanline.
		cachedDecoderMap 
			ifNil:[cachedDecoderMap := self rgbaDecoderMapForDepth: depth].
		(BitBlt toForm: form)
			sourceForm: ff;
			destOrigin: 0@y;
			combinationRule: Form over;
			colorMap: cachedDecoderMap;
			copyBits.
		^self.
	].
	tempForm := Form extent: width@1 depth: 32.
	tempBits := tempForm bits.
	pixel := LargePositiveInteger new: 4.
	i := -7.
	0 to: width-1 do: [ :x |
			i := i + 8.
			pixel at: 4 put: (thisScanline at: i+6);
				at: 3 put: (thisScanline at: i);
				at: 2 put: (thisScanline at: i+2);
				at: 1 put: (thisScanline at: i+4).
			tempBits at: x+1 put: pixel normalize.
	].
	tempForm displayOn: form at: 0@y rule: Form over.

]

{ #category : #'pixel copies' }
PNGReadWriter >> copyPixelsRGBA: y at: startX by: incX [
	"Handle interlaced RGBA color modes (colorType = 6)"

	| i pixel tempForm tempBits |

	tempForm := Form extent: width@1 depth: 32.
	tempBits := tempForm bits.
	pixel := LargePositiveInteger new: 4.
	bitsPerChannel = 8 ifTrue: [
		i := (startX // incX << 2) + 1.
		startX to: width-1 by: incX do: [ :x |
			pixel at: 4 put: (thisScanline at: i+3);
				at: 3 put: (thisScanline at: i);
				at: 2 put: (thisScanline at: i+1);
				at: 1 put: (thisScanline at: i+2).
			tempBits at: x+1 put: pixel normalize.
			i := i + 4.
		]
	] ifFalse: [
		i := (startX // incX << 3) +1.
		startX to: width-1 by: incX do: [ :x |
			pixel at: 4 put: (thisScanline at: i+6);
				at: 3 put: (thisScanline at: i);
				at: 2 put: (thisScanline at: i+2);
				at: 1 put: (thisScanline at: i+4).
			tempBits at: x+1 put: pixel normalize.
			i := i + 8.
		].
	].
	tempForm displayOn: form at: 0@y rule: Form paintAlpha.


]

{ #category : #accessing }
PNGReadWriter >> debugging [

	^Debugging == true
]

{ #category : #miscellaneous }
PNGReadWriter >> doPass: pass [
	"Certain interlace passes are skipped with certain small image
dimensions"

	pass = 1 ifTrue: [ ^ true ].
	((width = 1) and: [height = 1]) ifTrue: [ ^ false ].
	pass = 2 ifTrue: [ ^ width >= 5 ].
	pass = 3 ifTrue: [ ^ height >= 5 ].
	pass = 4 ifTrue: [ ^ (width >=3 ) or: [height >= 5] ].
	pass = 5 ifTrue: [ ^ height >=3 ].
	pass = 6 ifTrue: [ ^ width >=2 ].
	pass = 7 ifTrue: [ ^ height >=2 ].


]

{ #category : #filtering }
PNGReadWriter >> filterAverage: count [ 
	"Use the average of the pixel to the left and the pixel above as a predictor"
	| delta |
	delta := bitsPerPixel // 8 max: 1.
	1 
		to: delta
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + ((prevScanline at: i) // 2) bitAnd: 255) ].
	delta + 1 
		to: count
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + (((prevScanline at: i) + (thisScanline at: i - delta)) // 2) bitAnd: 255) ]
]

{ #category : #filtering }
PNGReadWriter >> filterHorizontal: count [ 
	"Use the pixel to the left as a predictor"
	| delta |
	delta := bitsPerPixel // 8 max: 1.
	delta + 1 
		to: count
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + (thisScanline at: i - delta) bitAnd: 255) ]
]

{ #category : #filtering }
PNGReadWriter >> filterNone: count [

]

{ #category : #filtering }
PNGReadWriter >> filterPaeth: count [ 
	"Select one of (the pixel to the left, the pixel above and the pixel to above left) to
	predict the value of this pixel"
	| delta |
	delta := bitsPerPixel // 8 max: 1.
	1 
		to: delta
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + (prevScanline at: i) bitAnd: 255) ].
	delta + 1 
		to: count
		do: 
			[ :i | 
			thisScanline 
				at: i
				put: ((thisScanline at: i) + (self 
						paethPredictLeft: (thisScanline at: i - delta)
						above: (prevScanline at: i)
						aboveLeft: (prevScanline at: i - delta)) bitAnd: 255) ]
]

{ #category : #filtering }
PNGReadWriter >> filterScanline: filterType count: count [

	self
		perform: (
			#(filterNone: filterHorizontal: filterVertical: filterAverage: filterPaeth:)
				at: filterType+1)
		with: count.


]

{ #category : #filtering }
PNGReadWriter >> filterVertical: count [
	"Use the pixel above as a predictor"

	1 to: count do: [ :i |
		thisScanline at: i put: (((thisScanline at: i) +
(prevScanline at: i)) bitAnd: 255) ]


]

{ #category : #miscellaneous }
PNGReadWriter >> grayColorsFor: d [
	"return a color table for a gray image"

	palette := Array new: 1<<d.
	d = 1 ifTrue: [
		palette at: 1 put: Color black.
		palette at: 2 put: Color white.
		^  palette
		].
	d = 2 ifTrue: [
		palette at: 1 put: Color black.
		palette at: 2 put: (Color gray: 85.0 / 255.0).
		palette at: 3 put: (Color gray: 170.0 / 255.0).
		palette at: 4 put: Color white.
		^ palette
		].
	d = 4 ifTrue: [
		0 to: 15 do: [ :g |
			palette at: g+1 put: (Color gray: (g/15) asFloat) ].
		^ palette
		].
	d = 8 ifTrue: [
		0 to: 255 do: [ :g |
			palette at: g+1 put: (Color gray: (g/255) asFloat) ].
		^ palette
		].

]

{ #category : #accessing }
PNGReadWriter >> nextImage [
	bigEndian := EndianDetector isBigEndian.
	filtersSeen := Bag new.
	idatChunkStream := nil.
	transparentPixelValue := nil.
	unknownChunks := Set new.
	stream skip: 8.
	[ stream atEnd ] whileFalse: [ self processNextChunk ].
	"Set up our form"
	palette
		ifNotNil: [ "Dump the palette if it's the same as our standard palette"
			palette = (StandardColors copyFrom: 1 to: palette size)
				ifTrue: [ palette := nil ] ].
	(depth <= 8 and: [ palette notNil ])
		ifTrue: [ form := ColorForm extent: width @ height depth: depth.
			form colors: palette ]
		ifFalse: [ form := Form extent: width @ height depth: depth ].
	backColor ifNotNil: [ form fillColor: backColor ].
	idatChunkStream
		ifNil: [ self error: 'image data is missing' ]
		ifNotNil: [ self processIDATChunk ].
	unknownChunks isEmpty
		ifFalse:
			[ "Transcript show: ' ',unknownChunks asSortedCollection asArray printString." ].
	self debugging
		ifTrue: [ self crTrace: 'form = ' , form printString.
			self crTrace: 'colorType = ' , colorType printString.
			self crTrace: 'interlaceMethod = ' , interlaceMethod printString.
			self
				crTrace: 'filters = ' , filtersSeen sortedCounts asArray printString ].
	^ form
]

{ #category : #writing }
PNGReadWriter >> nextPutImage: aForm [
	"Write out the given form. We're keeping it simple here, no interlacing, no filters."
	^self nextPutImage: aForm interlace: 0 filter: 0. "no filtering"
]

{ #category : #writing }
PNGReadWriter >> nextPutImage: aForm interlace: aMethod filter: aFilterType [ 
	"Note: For now we keep it simple - interlace and filtering are simply ignored"

	| crcStream |
	bigEndian := EndianDetector isBigEndian.
	form := aForm.
	width := aForm width.
	height := aForm height.
	aForm depth <= 8 
		ifTrue: 
			[bitsPerChannel := aForm depth.
			colorType := 3.
			bytesPerScanline := (width * aForm depth + 7) // 8]
		ifFalse: 
			[bitsPerChannel := 8.
			colorType := 6.
			bytesPerScanline := width * 4].
	self writeFileSignature.
	crcStream := (ByteArray new: 1000) writeStream.
	crcStream resetToStart.
	self writeIHDRChunkOn: crcStream.
	self writeChunk: crcStream.
	form depth <= 8 
		ifTrue: 
			[crcStream resetToStart.
			self writePLTEChunkOn: crcStream.
			self writeChunk: crcStream.
			form isColorForm 
				ifTrue: 
					[crcStream resetToStart.
					self writeTRNSChunkOn: crcStream.
					self writeChunk: crcStream]].
	form depth = 16 
		ifTrue: 
			[crcStream resetToStart.
			self writeSBITChunkOn: crcStream.
			self writeChunk: crcStream].
	crcStream resetToStart.
	self writeIDATChunkOn: crcStream.
	self writeChunk: crcStream.
	crcStream resetToStart.
	self writeIENDChunkOn: crcStream.
	self writeChunk: crcStream
]

{ #category : #filtering }
PNGReadWriter >> paethPredictLeft: a above: b aboveLeft: c [ 
	"Predicts the value of a pixel based on nearby pixels, based on
Paeth (GG II, 1991)"
	| pa pb pc |
	pa := b > c 
		ifTrue: [ b - c ]
		ifFalse: [ c - b ].
	pb := a > c 
		ifTrue: [ a - c ]
		ifFalse: [ c - a ].
	pc := a + b - c - c.
	pc < 0 ifTrue: [ pc := pc * -1 ].
	(pa <= pb and: [ pa <= pc ]) ifTrue: [ ^ a ].
	pb <= pc ifTrue: [ ^ b ].
	^ c
]

{ #category : #chunks }
PNGReadWriter >> processBackgroundChunk [
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
]

{ #category : #chunks }
PNGReadWriter >> processIDATChunk [

	interlaceMethod = 0
		ifTrue: [ self processNonInterlaced ]
		ifFalse: [ self processInterlaced ]

]

{ #category : #chunks }
PNGReadWriter >> processIHDRChunk [
	width := chunk 
		longAt: 1
		bigEndian: true.
	height := chunk 
		longAt: 5
		bigEndian: true.
	bitsPerChannel := chunk at: 9.
	colorType := chunk at: 10.
	"compression := chunk at: 11."	"TODO - validate compression"
	"filterMethod := chunk at: 12."	"TODO - validate filterMethod"
	interlaceMethod := chunk at: 13.	"TODO - validate interlace method"
	(#(2 4 6 ) includes: colorType) ifTrue: [ depth := 32 ].
	(#(0 3 ) includes: colorType) ifTrue: 
		[ depth := bitsPerChannel min: 8.
		colorType = 0 ifTrue: 
			[ "grayscale"
			palette := self grayColorsFor: depth ] ].
	bitsPerPixel := (BPP at: colorType + 1) at: bitsPerChannel highBit.
	bytesPerScanline := (width * bitsPerPixel + 7) // 8.
	rowSize := width * depth + 31 >> 5
]

{ #category : #chunks }
PNGReadWriter >> processInterlaced [
	| z filter bytesPerPass startingCol colIncrement rowIncrement startingRow cx sc temp |
	startingCol := #(0 4 0 2 0 1 0 ).
	colIncrement := #(8 8 4 4 2 2 1 ).
	rowIncrement := #(8 8 8 4 4 2 2 ).
	startingRow := #(0 0 4 0 2 0 1 ).
 	z := ZLibReadStream 
 		on: idatChunkStream originalContents
 		from: 1
 		to: idatChunkStream position.
	1 
		to: 7
		do: 
			[ :pass | 
			(self doPass: pass) ifTrue: 
				[ cx := colIncrement at: pass.
				sc := startingCol at: pass.
				bytesPerPass := ((width - sc + cx - 1) // cx * bitsPerPixel + 7) // 8.
				prevScanline := ByteArray new: bytesPerPass.
				thisScanline := ByteArray new: bytesPerScanline.
				(startingRow at: pass) 
					to: height - 1
					by: (rowIncrement at: pass)
					do: 
						[ :y | 
						filter := z next.
						filtersSeen add: filter.
						(filter isNil or: [ (filter 
							between: 0
							and: 4) not ]) ifTrue: [ ^ self ].
						thisScanline := z 
							next: bytesPerPass
							into: thisScanline
							startingAt: 1.
						self 
							filterScanline: filter
							count: bytesPerPass.
						self 
							copyPixels: y
							at: sc
							by: cx.
						temp := prevScanline.
						prevScanline := thisScanline.
						thisScanline := temp ] ] ].
	z atEnd ifFalse: [ self error: 'Unexpected data' ]
]

{ #category : #chunks }
PNGReadWriter >> processNextChunk [
	| length chunkType crc chunkCrc |
	length := self nextLong.
	chunkType := (self next: 4) asString.
 	(chunk isNil or: [ chunk size ~= length ])
 		ifTrue: [ chunk := self next: length ]
 		ifFalse: [ stream next: length into: chunk startingAt: 1 ].
	chunkCrc := self nextLong bitXor: 4294967295.
	crc := self 
		updateCrc: 4294967295
		from: 1
		to: 4
		in: chunkType.
	crc := self 
		updateCrc: crc
		from: 1
		to: length
		in: chunk.
	crc = chunkCrc ifFalse: [ self error: 'PNGReadWriter crc error in chunk ' , chunkType ].
	chunkType = 'IEND' ifTrue: [ ^ self	"*should* be the last chunk" ].
	chunkType = 'sBIT' ifTrue: 
		[ ^ self processSBITChunk	"could indicate unusual sample depth in original" ].
	chunkType = 'gAMA' ifTrue: [ ^ self	"indicates gamma correction value" ].
	chunkType = 'bKGD' ifTrue: [ ^ self processBackgroundChunk ].
	chunkType = 'pHYs' ifTrue: [ ^ self processPhysicalPixelChunk ].
	chunkType = 'tRNS' ifTrue: [ ^ self processTransparencyChunk ].
	chunkType = 'IHDR' ifTrue: [ ^ self processIHDRChunk ].
	chunkType = 'PLTE' ifTrue: [ ^ self processPLTEChunk ].
	chunkType = 'IDAT' ifTrue: 
		[ "---since the compressed data can span multiple
		chunks, stitch them all together first. later,
		if memory is an issue, we need to figure out how
		to do this on the fly---"
 		idatChunkStream
 			ifNil: [ idatChunkStream := WriteStream with: chunk copy ]
 			ifNotNil: [ idatChunkStream nextPutAll: chunk ].
		^ self ].
	unknownChunks add: chunkType
]

{ #category : #chunks }
PNGReadWriter >> processNonInterlaced [
	| z filter temp copyMethod debug |
	debug := self debugging.
	copyMethod := #(#copyPixelsGray: nil #copyPixelsRGB: #copyPixelsIndexed: #copyPixelsGrayAlpha: nil #copyPixelsRGBA:)
		at: colorType + 1.
	debug
		ifTrue: [ self
				crTrace:
					('NI chunk size=<1s>' expandMacrosWith: idatChunkStream position) ].
	z := ZLibReadStream
		on: idatChunkStream originalContents
		from: 1
		to: idatChunkStream position.
	prevScanline := ByteArray new: bytesPerScanline.
	thisScanline := ByteArray new: bytesPerScanline.
	0 to: height - 1 do: [ :y | 
		filter := z next.
		debug
			ifTrue: [ filtersSeen add: filter ].
		thisScanline := z
			next: bytesPerScanline
			into: thisScanline
			startingAt: 1.
		(debug and: [ thisScanline size < bytesPerScanline ])
			ifTrue: [ self
					traceCr:
						('wanted {1} but only got {2}'
							format:
								{bytesPerScanline.
								thisScanline size}) ].
		filter = 0
			ifFalse: [ self filterScanline: filter count: bytesPerScanline ].
		self perform: copyMethod with: y.
		temp := prevScanline.
		prevScanline := thisScanline.
		thisScanline := temp ].
	z atEnd
		ifFalse: [ self error: 'Unexpected data' ].
	debug
		ifTrue: [ self trace: ' compressed size=' , z position asString ]
]

{ #category : #chunks }
PNGReadWriter >> processPLTEChunk [
	| colorCount i |
	colorCount := chunk size // 3.	"TODO - validate colorCount against depth"
	palette := Array new: colorCount.
	0 
		to: colorCount - 1
		do: 
			[ :index | 
			i := index * 3 + 1.
			palette 
				at: index + 1
				put: (Color 
						r: (chunk at: i) / 255.0
						g: (chunk at: i + 1) / 255.0
						b: (chunk at: i + 2) / 255.0) ]
]

{ #category : #chunks }
PNGReadWriter >> processPhysicalPixelChunk [

	"Transcript show: '  PHYSICAL: ',chunk printString."

]

{ #category : #chunks }
PNGReadWriter >> processSBITChunk [
	| rBits gBits bBits aBits |
	colorType = 6 ifFalse:[^self].
	rBits := chunk at: 1.
	gBits := chunk at: 2.
	bBits := chunk at: 3.
	aBits := chunk at: 4.
	(rBits = 5 and:[gBits = 5 and:[bBits = 5 and:[aBits = 1]]]) ifTrue:[
		depth := 16.
	].
]

{ #category : #chunks }
PNGReadWriter >> processTransparencyChunk [

	"Transcript show: '  TRANSPARENCY ',chunk printString."
	colorType = 0
		ifTrue:
			[transparentPixelValue := chunk unsignedShortAt: 1 bigEndian: true.
			bitsPerChannel <= 8
				ifTrue: [palette at: transparentPixelValue + 1 put: Color transparent]
				ifFalse: [palette at: 1 put: Color transparent].
			^self].
	colorType = 2
		ifTrue:
			[| red green blue |
			red :=  chunk unsignedShortAt: 1 bigEndian: true.
			green :=  chunk unsignedShortAt: 3 bigEndian: true.
			blue :=  chunk unsignedShortAt: 5 bigEndian: true.
			transparentPixelValue := bitsPerChannel <= 8
				ifTrue: [16rFF00 + red << 8 + green << 8 + blue]
				ifFalse: [red << 16 + green << 16 + blue].
			^self].
	colorType = 3
		ifTrue:
			[chunk withIndexDo: [ :alpha :index |
				palette at: index put: ((palette at: index) alpha: alpha/255)].
			^self].

]

{ #category : #'pixel copies' }
PNGReadWriter >> rgbaDecoderMapForDepth: decoderDepth [
	bigEndian ifTrue:[
		depth = 16 ifTrue:[
			"Big endian, 32 -> 16 color mapping."
			^ColorMap
				shifts: #(-17 -14 -11 0)
				masks: #(16rF8000000 16rF80000 16rF800 16r00)
		] ifFalse:[
			"Big endian, 32 -> 32 color mapping"
			^ColorMap 
				shifts: #(-8 -8 -8 24) 
				masks: #(16rFF000000 16rFF0000 16rFF00 16rFF).
		].
	].
	depth = 16 ifTrue:[
		"Little endian, 32 -> 16 color mapping."
		^ColorMap
			shifts: #(7 -6 -19 0)
			masks: #(16rF8 16rF800 16rF80000 0)
	] ifFalse:[
		"Little endian, 32 -> 32 color mapping"
		^ColorMap 
			shifts: #(-16 0 16 0) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000).
	].
]

{ #category : #accessing }
PNGReadWriter >> understandsImageFormat [
	#(137 80 78 71 13 10 26 10) do: [ :byte |
		stream next = byte ifFalse: [^ false]].
	^ true

]

{ #category : #writing }
PNGReadWriter >> updateCrc: oldCrc from: start to: stop in: aCollection [
	^ZipWriteStream updateCrc: oldCrc from: start to: stop in: aCollection
]

{ #category : #writing }
PNGReadWriter >> writeChunk: crcStream [
	| bytes length crc debug |
	debug := self debugging.
	bytes := crcStream originalContents.
	length := crcStream position.
	crc := self
		updateCrc: 4294967295
		from: 1
		to: length
		in: bytes.
	crc := crc bitXor: 4294967295.
	debug
		ifTrue: [ self
				crTrace:
					(String
						streamContents: [ :stream | 
							stream
								cr;
								print: stream position;
								space;
								nextPutAll: (bytes copyFrom: 1 to: 4) asString;
								nextPutAll: ' len=';
								print: length;
								nextPutAll: ' crc=0x';
								nextPutAll: crc printStringHex ]) ].
	self nextLongPut: length - 4.	"exclude chunk name"
	stream next: length putAll: bytes startingAt: 1.
	self nextLongPut: crc.
	debug
		ifTrue: [ self crTrace: ' afterPos=' , stream position asString ].
	crcStream resetToStart
]

{ #category : #writing }
PNGReadWriter >> writeFileSignature [
	stream nextPutAll: #[16r89 16r50 16r4E  16r47 16r0D 16r0A 16r1A 16r0A]
]

{ #category : #writing }
PNGReadWriter >> writeIDATChunkOn: aStream [
	"Write the IDAT chunk"

	| z |
	aStream nextPutAll: 'IDAT' asByteArray.
	z := ZLibWriteStream on: aStream.
	form depth <= 8
		ifTrue: [ self writeType3DataOn: z ]
		ifFalse: [ self writeType6DataOn: z ].
	self debugging
		ifTrue: [ self
				crTrace:
					('compressed size=<1s> uncompressed size=<2s>'
						expandMacrosWith: aStream position
						with: z position) ]
]

{ #category : #writing }
PNGReadWriter >> writeIENDChunkOn: aStream [
	"Write the IEND chunk"
	aStream nextPutAll: 'IEND' asByteArray.
]

{ #category : #writing }
PNGReadWriter >> writeIHDRChunkOn: aStream [
	"Write the IHDR chunk"
	aStream nextPutAll: 'IHDR' asByteArray.
	aStream nextInt32Put: width.
	aStream nextInt32Put: height.
	aStream nextNumber: 1 put: bitsPerChannel.
	aStream nextNumber: 1 put: colorType.
	aStream nextNumber: 1 put: 0. "compression"
	aStream nextNumber: 1 put: 0. "filter method"
	aStream nextNumber: 1 put: 0. "interlace method"

]

{ #category : #writing }
PNGReadWriter >> writePLTEChunkOn: aStream [
	"Write the PLTE chunk"
	| colors |
	aStream nextPutAll: 'PLTE' asByteArray.
	(form isColorForm) 
		ifTrue:[colors := form colors]
		ifFalse:[colors := Color indexedColors copyFrom: 1 to: (1 bitShift: form depth)].
	colors do:[:aColor| | r g b |
		r := (aColor red * 255) truncated.
		g := (aColor green * 255) truncated.
		b := (aColor blue * 255) truncated.
		aStream nextPut: r; nextPut: g; nextPut: b.
	].
]

{ #category : #writing }
PNGReadWriter >> writeSBITChunkOn: aStream [
	"Write the IDAT chunk"
	aStream nextPutAll: 'sBIT' asByteArray.
	form depth = 16 ifFalse:[self error: 'Unimplemented feature'].
	aStream nextPut: 5.
	aStream nextPut: 5.
	aStream nextPut: 5.
	aStream nextPut: 1.
]

{ #category : #writing }
PNGReadWriter >> writeTRNSChunkOn: aStream [
	"Write out tRNS chunk"
	aStream nextPutAll: 'tRNS' asByteArray.
	form colors do:[:aColor|
		aStream nextPut: (aColor alpha * 255) truncated.
	].
]

{ #category : #writing }
PNGReadWriter >> writeType3DataOn: zStream [
	"Write color indexed data."
	| scanline hack hackBlt swizzleBlt swizzleHack hackDepth |
	scanline := ByteArray new: bytesPerScanline + 3 // 4 * 4.
	hackDepth := bigEndian ifTrue:[form depth] ifFalse:[form depth negated].
	hack := Form extent: width@1 depth: hackDepth bits: scanline.
	hackBlt := BitBlt toForm: hack.
	hackBlt sourceForm: form.
	hackBlt combinationRule: Form over.
	hackBlt destOrigin: 0@0.
	hackBlt width: width; height: 1.
	(form depth < 8 and:[bigEndian not]) ifTrue:[
		swizzleHack := Form new hackBits: scanline.
		swizzleBlt := BitBlt toForm: swizzleHack.
		swizzleBlt sourceForm: swizzleHack.
		swizzleBlt combinationRule: Form over.
		swizzleBlt colorMap: (StandardSwizzleMaps at: form depth).
	].
	0 to: height-1 do:[:i|
		hackBlt sourceOrigin: 0@i; copyBits.
		swizzleBlt ifNotNil:[swizzleBlt copyBits].
		zStream nextPut: 0. "filterType"
		zStream next: bytesPerScanline putAll: scanline startingAt: 1.
	].
	zStream close.
]

{ #category : #writing }
PNGReadWriter >> writeType6DataOn: zStream [
	"Write RGBA data."
	| scanline hack hackBlt cm miscBlt |
	scanline := ByteArray new: bytesPerScanline.
	hack := Form extent: width@1 depth: 32 bits: scanline.
	form depth = 16 ifTrue:[
		"Expand 16 -> 32"
		miscBlt := BitBlt toForm: hack.
		miscBlt sourceForm: form.
		miscBlt combinationRule: Form over.
		miscBlt destOrigin: 0@0.
		miscBlt width: width; height: 1.
	].
	hackBlt := BitBlt toForm: hack.
	hackBlt sourceForm: (miscBlt ifNil:[form] ifNotNil:[hack]).
	hackBlt combinationRule: Form over.
	hackBlt destOrigin: 0@0.
	hackBlt width: width; height: 1.
	bigEndian ifTrue:[
		cm := ColorMap 
			shifts: #(8 8 8 -24) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000).
	] ifFalse:[
		cm := ColorMap 
			shifts: #(-16 0 16 0) 
			masks: #(16rFF0000 16rFF00 16rFF 16rFF000000).
	].
	hackBlt colorMap: cm.
	0 to: height-1 do:[:i|
		miscBlt ifNil:[
			hackBlt sourceOrigin: 0@i; copyBits.
		] ifNotNil:[
			miscBlt sourceOrigin: 0@i; copyBits.
			hack fixAlpha.
			hackBlt copyBits.
		].
		zStream nextPut: 0. "filterType"
		zStream nextPutAll: scanline.
	].
	zStream close.
]
