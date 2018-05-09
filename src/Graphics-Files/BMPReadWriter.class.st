"
I read and write BMP files.

Example to save and load a screenshot of the world in a .bmp file:

BMPReadWriter 
   putForm: (Form fromDisplay: (0@0 corner: 400@400))
   onFileNamed: '/tmp/screenshot.bmp'.

(ImageMorph withForm: (BMPReadWriter formFromFileNamed: '/tmp/screenshot.bmp')) openInWindow.
"
Class {
	#name : #BMPReadWriter,
	#superclass : #ImageReadWriter,
	#instVars : [
		'bfType',
		'bfSize',
		'bfOffBits',
		'biSize',
		'biWidth',
		'biHeight',
		'biPlanes',
		'biBitCount',
		'biCompression',
		'biSizeImage',
		'biXPelsPerMeter',
		'biYPelsPerMeter',
		'biClrUsed',
		'biClrImportant'
	],
	#category : #'Graphics-Files'
}

{ #category : #testing }
BMPReadWriter class >> displayAllFrom: fd [
	"BMPReadWriter displayAllFrom: FileDirectory default"
	fd fileNames do:[:fName|
		(fName endsWith: '.bmp') ifTrue:[
			[(Form fromBinaryStream: (fd readOnlyFileNamed: fName)) display.
			Display forceDisplayUpdate] on: Error do:[:nix|].
		].
	].
	fd directoryNames do:[:fdName|
		self displayAllFrom: (fd / fdName)
	].
]

{ #category : #testing }
BMPReadWriter class >> readAllFrom: fd [
	"MessageTally spyOn:[BMPReadWriter readAllFrom: FileDirectory default]"
	fd fileNames do:[:fName|
		(fName endsWith: '.bmp') ifTrue:[
			[Form fromBinaryStream: (fd readOnlyFileNamed: fName)] on: Error do:[:nix].
		].
	].
	fd directoryNames do:[:fdName|
		self readAllFrom: (fd / fdName)
	].
]

{ #category : #'image reading/writing' }
BMPReadWriter class >> typicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that I can read might commonly have"
	^#('bmp')
]

{ #category : #reading }
BMPReadWriter >> nextImage [
	| colors |
	self readHeader.
	biBitCount = 24 ifTrue:[^self read24BmpFile].
	"read the color map"
	colors := self readColorMap.
	^self readIndexedBmpFile: colors
]

{ #category : #writing }
BMPReadWriter >> nextPutImage: aForm [
	| bhSize rowBytes rgb data colorValues depth image ppw scanLineLen pixline |
	depth := aForm depth.
	depth := #(1 4 8 32 ) detect: [ :each | each >= depth].
	image := aForm asFormOfDepth: depth.
	image unhibernate.
	bhSize := 14.  "# bytes in file header"
	biSize := 40.  "info header size in bytes"
	biWidth := image width.
	biHeight := image height.
	biClrUsed := depth = 32 ifTrue: [0] ifFalse:[1 << depth].  "No. color table entries"
	bfOffBits := biSize + bhSize + (4*biClrUsed).
	rowBytes := ((depth min: 24) * biWidth + 31 // 32) * 4.
	biSizeImage := biHeight * rowBytes.

	"Write the file header"
	self nextLittleEndianNumber: 2 put: 19778.  "bfType = BM"
	self nextLittleEndianNumber: 4 put: bfOffBits + biSizeImage.  "Entire file size in bytes"
	self nextLittleEndianNumber: 4 put: 0.  "bfReserved"
	self nextLittleEndianNumber: 4 put: bfOffBits.  "Offset of bitmap data from start of hdr (and file)"

	"Write the bitmap info header"
	self nextLittleEndianNumber: 4 put: biSize.  "info header size in bytes"
	self nextLittleEndianNumber: 4 put: image width.  "biWidth"
	self nextLittleEndianNumber: 4 put: image height.  "biHeight"
	self nextLittleEndianNumber: 2 put: 1.  "biPlanes"
	self nextLittleEndianNumber: 2 put: (depth min: 24).  "biBitCount"
	self nextLittleEndianNumber: 4 put: 0.  "biCompression"
	self nextLittleEndianNumber: 4 put: biSizeImage.  "size of image section in bytes"
	self nextLittleEndianNumber: 4 put: 2800.  "biXPelsPerMeter"
	self nextLittleEndianNumber: 4 put: 2800.  "biYPelsPerMeter"
	self nextLittleEndianNumber: 4 put: biClrUsed.
	self nextLittleEndianNumber: 4 put: 0.  "biClrImportant"
	biClrUsed > 0 ifTrue: [
		"write color map; this works for ColorForms, too"
		colorValues := image colormapIfNeededForDepth: 32.
		1 to: biClrUsed do: [:i |
			rgb := colorValues at: i.
			0 to: 24 by: 8 do: [:j | stream nextPut: (rgb >> j bitAnd: 16rFF)]]].

	depth < 32 ifTrue: [
		"depth = 1, 4 or 8."
		data := image bits asByteArray.
		ppw := 32 // depth.
		scanLineLen := biWidth + ppw - 1 // ppw * 4.  "# of bytes in line"
		1 to: biHeight do: [:i |
			stream next: scanLineLen putAll: data startingAt: (biHeight-i)*scanLineLen+1.
		].
	] ifFalse: [
		data := image bits.
		pixline := ByteArray new: (((biWidth * 3 + 3) // 4) * 4).
		1 to: biHeight do:[:i |
			self store24BitBmpLine: pixline from: data startingAt: (biHeight-i)*biWidth+1 width: biWidth.
			stream nextPutAll: pixline.
		].
	]
]

{ #category : #reading }
BMPReadWriter >> read24BmpFile [
	"Read 24-bit pixel data from the given a BMP stream."
	| form formBits pixelLine bitsIndex bitBlt |
	form := Form 
		extent: biWidth @ biHeight
		depth: 32.
	pixelLine := ByteArray new: (24 * biWidth + 31) // 32 * 4.
	bitsIndex := (form height - 1) * biWidth + 1.
	formBits := form bits.
	1 
		to: biHeight
		do: 
			[ :i | 
			pixelLine := stream nextInto: pixelLine.
			self 
				read24BmpLine: pixelLine
				into: formBits
				startingAt: bitsIndex
				width: biWidth.
			bitsIndex := bitsIndex - biWidth ].
	bitBlt := BitBlt toForm: form.
	bitBlt combinationRule: 7.	"bitOr:with:"
	bitBlt halftoneForm: (Bitmap with: 4278190080).
	bitBlt copyBits.
	^ form
]

{ #category : #reading }
BMPReadWriter >> read24BmpLine: pixelLine into: formBits startingAt: formBitsIndex width: width [
	"Swizzles the bytes in a 24bpp scanline and fills in the given 32bpp form bits. Ensures that color black is represented as 16rFF000001 so that Form paint works properly."
	<primitive: 'primitiveRead24BmpLine' module:'BMPReadWriterPlugin'>
	
	| pixIndex rgb bitsIndex |
	pixIndex := 0. "pre-increment"
	bitsIndex := formBitsIndex-1. "pre-increment"
	1 to: width do: [:j |
		rgb :=  (pixelLine at: (pixIndex := pixIndex+1)) +
				((pixelLine at: (pixIndex := pixIndex+1)) bitShift: 8) +
				((pixelLine at: (pixIndex := pixIndex+1)) bitShift: 16).
		rgb = 0 ifTrue:[rgb := 16rFF000001] ifFalse:[rgb := rgb + 16rFF000000].
		formBits at: (bitsIndex := bitsIndex+1) put: rgb].

]

{ #category : #reading }
BMPReadWriter >> readColorMap [
	"Read colorCount BMP color map entries from the given binary stream. Answer an array of Colors."
	| colorCount colors maxLevel b g r ccStream |
	colorCount := (bfOffBits - 54) // 4.
	"Note: some programs (e.g. Photoshop 4.0) apparently do not set colorCount; assume that any data between the end of the header and the start of the pixel data is the color map"
	biBitCount >= 16 ifTrue: [ ^ nil ].
	colorCount = 0 ifTrue: 
		[ "this BMP file does not have a color map"
		"default monochrome color map"
		biBitCount = 1 ifTrue: 
			[ ^ Array 
				with: Color white
				with: Color black ].
		"default gray-scale color map"
		maxLevel := (2 raisedTo: biBitCount) - 1.
		^ (0 to: maxLevel) collect: [ :level | Color gray: level asFloat / maxLevel ] ].
	ccStream := (stream next: colorCount * 4) readStream.
	colors := Array new: colorCount.
	1 
		to: colorCount
		do: 
			[ :i | 
			b := ccStream next.
			g := ccStream next.
			r := ccStream next.
			ccStream next.	"skip reserved"
			colors 
				at: i
				put: (Color 
						r: r
						g: g
						b: b
						range: 255) ].
	^ colors
]

{ #category : #reading }
BMPReadWriter >> readHeader [
	| reserved |
	bfType := self nextLittleEndianNumber: 2.
	bfSize := self nextLittleEndianNumber: 4.
	reserved := self nextLittleEndianNumber: 4.
	bfOffBits := self nextLittleEndianNumber: 4.
	biSize := self nextLittleEndianNumber: 4.
	biWidth := self nextLittleEndianNumber: 4.
	biHeight := self nextLittleEndianNumber: 4.
	biPlanes := self nextLittleEndianNumber: 2.
	biBitCount := self nextLittleEndianNumber: 2.
	biCompression := self nextLittleEndianNumber: 4.
	biSizeImage := self nextLittleEndianNumber: 4.
	biXPelsPerMeter := self nextLittleEndianNumber: 4.
	biYPelsPerMeter := self nextLittleEndianNumber: 4.
	biClrUsed := self nextLittleEndianNumber: 4.
	biClrImportant := self nextLittleEndianNumber: 4
]

{ #category : #reading }
BMPReadWriter >> readIndexedBmpFile: colors [ 
	"Read uncompressed pixel data of depth d from the given BMP stream, where d is 1, 4, 8, or 16"
	| form bytesPerRow pixelData pixelLine startIndex map bitBlt mask |
	colors 
		ifNil: 
			[ form := Form 
				extent: biWidth @ biHeight
				depth: biBitCount ]
		ifNotNil: 
			[ form := ColorForm 
				extent: biWidth @ biHeight
				depth: biBitCount.
			form colors: colors ].
	bytesPerRow := (biBitCount * biWidth + 31) // 32 * 4.
	pixelData := ByteArray new: bytesPerRow * biHeight.
	biHeight 
		to: 1
		by: -1
		do: 
			[ :y | 
			pixelLine := stream next: bytesPerRow.
			startIndex := (y - 1) * bytesPerRow + 1.
			pixelData 
				replaceFrom: startIndex
				to: startIndex + bytesPerRow - 1
				with: pixelLine
				startingAt: 1 ].
	form bits copyFromByteArray: pixelData.
	biBitCount = 16 ifTrue: 
		[ map := ColorMap 
			shifts: #(8 -8 0 0 )
			masks: #(255 65280 0 0 ).
		mask := 2147516416 ].
	biBitCount = 32 ifTrue: 
		[ map := ColorMap 
			shifts: #(24 8 -8 -24 )
			masks: #(255 65280 16711680 4278190080 ).
		mask := 4278190080 ].
	map ifNotNil: 
		[ bitBlt := BitBlt toForm: form.
		bitBlt sourceForm: form.
		bitBlt colorMap: map.
		bitBlt combinationRule: Form over.
		bitBlt copyBits ].
	mask ifNotNil: 
		[ bitBlt := BitBlt toForm: form.
		bitBlt combinationRule: 7.	"bitOr:with:"
		bitBlt halftoneForm: (Bitmap with: mask).
		bitBlt copyBits ].
	^ form
]

{ #category : #writing }
BMPReadWriter >> store24BitBmpLine: pixelLine from: formBits startingAt: formBitsIndex width: width [
	"Stores a single scanline containing 32bpp RGBA values in a 24bpp scanline. Swizzles the bytes as needed."
	<primitive: 'primitiveWrite24BmpLine' module:'BMPReadWriterPlugin'>

	| pixIndex rgb bitsIndex |	
	pixIndex := 0. "pre-increment"
	bitsIndex := formBitsIndex-1. "pre-increment"
	1 to: width do: [:j |
		rgb := (formBits at: (bitsIndex := bitsIndex+1)) bitAnd: 16rFFFFFF.
		pixelLine at: (pixIndex := pixIndex+1) put: (rgb bitAnd: 255).
		pixelLine at: (pixIndex := pixIndex+1) put: ((rgb bitShift: -8) bitAnd: 255).
		pixelLine at: (pixIndex := pixIndex+1) put: ((rgb bitShift: -16) bitAnd: 255).
	].

]

{ #category : #testing }
BMPReadWriter >> understandsImageFormat [
	self readHeader.
	bfType = 19778 "BM" ifFalse:[^false].
	biSize = 40 ifFalse:[^false].
	biPlanes = 1 ifFalse:[^false].
	biCompression = 0 ifFalse:[^false].
	^true
]
