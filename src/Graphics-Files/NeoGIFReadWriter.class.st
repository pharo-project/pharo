"
Copyright (c) Kazuki Yasumatsu, 1995. All rights reserved.

Used with permission.  Modified for use in Squeak.
"
Class {
	#name : #NeoGIFReadWriter,
	#superclass : #ImageReadWriter,
	#instVars : [
		'width',
		'height',
		'bitsPerPixel',
		'colorPalette',
		'rowByteSize',
		'xpos',
		'ypos',
		'pass',
		'interlace',
		'codeSize',
		'clearCode',
		'eoiCode',
		'freeCode',
		'maxCode',
		'prefixTable',
		'suffixTable',
		'remainBitCount',
		'bufByte',
		'bufStream',
		'transparentIndex',
		'mapOf32',
		'localColorTable',
		'loopCount',
		'offset',
		'frames',
		'canvasWidth',
		'canvasHeight',
		'backgroundColorIndex'
	],
	#classVars : [
		'Extension',
		'ImageSeparator',
		'Terminator'
	],
	#category : #'Graphics-Files'
}

{ #category : #examples }
NeoGIFReadWriter class >> exampleAnim [
	"This example writes out an animated gif of
	 a red circle"

	| writer extent center |
	writer := NeoGIFReadWriter on: (File openForWriteFileNamed: 'anim.gif').
	writer loopCount: 20.		"Repeat 20 times"
	writer delay: 10.		"Wait 10/100 seconds"
	extent := 42@42.
	center := extent / 2.
	Cursor write showWhile: [
		[2 to: center x - 1 by: 2 do: [:r |
			"Make a fancy anim without using Canvas - inefficient as hell"
			| image |
			image := ColorForm extent: extent depth: 8.
			0.0 to: 359.0 do: [:theta | image colorAt: (center + (Point r: r degrees: theta)) rounded put: Color red].
			writer nextPutImage: image]
		]	ensure: [writer close]].
]

{ #category : #examples }
NeoGIFReadWriter class >> grabScreenAndSaveOnDisk [
	"GIFReadWriter grabScreenAndSaveOnDisk"
	| form fileName |
	
	form := Form fromUser.
	form bits size = 0 ifTrue: [ ^ self inform: 'Empty region selected.' ].
	
	fileName := (FileSystem disk workingDirectory / 'Pharo', 'gif') fullName.
		
	UIManager default 
		informUser: 'Writing ' translated, fileName
		during: 
			[ NeoGIFReadWriter 
				putForm: form
				onFileNamed: fileName ]
]

{ #category : #initialization }
NeoGIFReadWriter class >> initialize [
	"GIFReadWriter initialize"
	ImageSeparator := $, asInteger.
	Extension := $! asInteger.
	Terminator := $; asInteger
]

{ #category : #'image reading/writing' }
NeoGIFReadWriter class >> typicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that I can 
	read might commonly have"

	^ self allSubclasses
		detect: [ :cls | cls wantsToHandleGIFs ]
		ifFound: [ #() ]
		ifNone: [ 
			"if none of my subclasses wants , then i''ll have to do"
			#('gif') ]
]

{ #category : #'image reading/writing' }
NeoGIFReadWriter class >> wantsToHandleGIFs [
	^ true
]

{ #category : #'as yet unclassified' }
NeoGIFReadWriter >> altReadBitData [
	"using modified Lempel-Ziv Welch algorithm."
	| outColors outCount initCodeSize  packedBits hasLocalColor localColorSize maxOutCodes decoder outCodes colorMapStream c outBytes bytes f colorIndices activeColorTable indexStream |
	maxOutCodes := 4096.
	offset := self readWord @ self readWord.	"Image Left@Image Top"
	width := self readWord.
	height := self readWord.

	"---
	Local Color Table Flag        1 Bit
	Interlace Flag                1 Bit
	Sort Flag                     1 Bit
	Reserved                      2 Bits
	Size of Local Color Table     3 Bits
	----"
	packedBits := self next.
	interlace := (packedBits bitAnd: 64) ~= 0.
	hasLocalColor := (packedBits bitAnd: 128) ~= 0.
	localColorSize := 1 bitShift: (packedBits bitAnd: 7) + 1.
	activeColorTable := colorPalette.
	hasLocalColor ifTrue: [ 
		localColorTable := self readColorTable: localColorSize.
		activeColorTable := localColorTable ].
	pass := 0.
	xpos := 0.
	ypos := 0.
	rowByteSize := (width + 3) // 4 * 4.
	bytes := ByteArray new: rowByteSize * height.
	remainBitCount := 0.
	bufByte := 0.
	bufStream := ByteArray new readStream.
	outCodes := ByteArray new: maxOutCodes + 1.
	outCount := 0.
	prefixTable := Array new: 4096.
	suffixTable := Array new: 4096.
	initCodeSize := self next.
	decoder := NeoLzwGifDecoder new.
	"hasLocalColor
		ifTrue: [ 
			decoder colorTable: localColorTable ]
		ifFalse: [ 
			decoder colorTable: colorPalette  ]".
	c := ColorForm 
		extent: width@height
		depth: 8. "Change to bitsPerPixel?"
	c colors: activeColorTable.
	decoder 
		codeStream: stream;
		minimumCodeSize: initCodeSize;
		maxCode: 4096.
	colorIndices := decoder decode.
	indexStream := colorIndices readStream.
	"colorIndices do: [ :d |
		bytes
			at: (ypos * rowByteSize + xpos + 1)
			put: d.
		self updatePixelPosition ]."
	"c bits copyFromByteArray: bytes."
	0 to: (width - 1) do: [ :y |
		0 to: (height - 1) do: [ :x |
			| idx nextColor |
			idx := indexStream next.
			nextColor := activeColorTable at: (idx + 1).
			c colorAt: (x@y) put: nextColor ] ].
	f := Form
		extent: c extent
		depth: bitsPerPixel.
	^ c displayOn: f.
]

{ #category : #'as yet unclassified' }
NeoGIFReadWriter >> altReadBitDataOnFrame: aFrame [
	"using modified Lempel-Ziv Welch algorithm."
	| initCodeSize  packedBits hasLocalColor localColorSize maxOutCodes decoder c  bytes |
	maxOutCodes := 4096.
	offset := self readWord @ self readWord.	"Image Left@Image Top"
	width := self readWord.
	height := self readWord.

	"---
	Local Color Table Flag        1 Bit
	Interlace Flag                1 Bit
	Sort Flag                     1 Bit
	Reserved                      2 Bits
	Size of Local Color Table     3 Bits
	----"
	packedBits := self next.
	interlace := (packedBits bitAnd: 64) ~= 0.
	hasLocalColor := (packedBits bitAnd: 128) ~= 0.
	localColorSize := 1 bitShift: (packedBits bitAnd: 7) + 1.
	hasLocalColor ifTrue: [ 
		localColorTable := self readColorTable: localColorSize ].
	pass := 0.
	xpos := 0.
	ypos := 0.
	rowByteSize := (width + 3) // 4 * 4.
	bytes := ByteArray new: rowByteSize * height.

	initCodeSize := self next.

	c := ColorForm 
		extent: width@height
		depth: 8. 

	decoder := NeoLzwGifDecoder new.
	decoder 
		codeStream: stream;
		minimumCodeSize: initCodeSize;
		maxCode: maxOutCodes;
		onDecodedBit: [ :bit |
			bytes
				at: (ypos * rowByteSize + xpos + 1)
				put: bit.
			self updatePixelPosition ].
	decoder decode.
	c bits copyFromByteArray: bytes.
	^ c
]

{ #category : #accessing }
NeoGIFReadWriter >> backgroundColor [
	backgroundColorIndex ifNotNil: [ 
		colorPalette ifNotNil: [ 
			^ colorPalette at: backgroundColorIndex + 1]].
	^ Color transparent.
]

{ #category : #accessing }
NeoGIFReadWriter >> canvasHeight [
	^ canvasHeight
]

{ #category : #accessing }
NeoGIFReadWriter >> canvasHeight: aNumber [
	canvasHeight := aNumber
]

{ #category : #accessing }
NeoGIFReadWriter >> canvasWidth [
	^ canvasWidth
]

{ #category : #accessing }
NeoGIFReadWriter >> canvasWidth: aNumber [
	canvasWidth := aNumber
]

{ #category : #private }
NeoGIFReadWriter >> checkCodeSize [
	(freeCode > maxCode and: [ codeSize < 12 ]) ifTrue: 
		[ codeSize := codeSize + 1.
		maxCode := (1 bitShift: codeSize) - 1 ]
]

{ #category : #'stream access' }
NeoGIFReadWriter >> close [
	"Write terminator"
	self nextPut: Terminator.
	^super close
]

{ #category : #'private-packing' }
NeoGIFReadWriter >> fillBuffer [
	| packSize |
	packSize := self next.
	bufStream := (self next: packSize) readStream
]

{ #category : #'private-bits access' }
NeoGIFReadWriter >> flushBits [
	remainBitCount = 0 ifFalse: 
		[ self nextBytePut: bufByte.
		remainBitCount := 0 ].
	self flushBuffer
]

{ #category : #'private-packing' }
NeoGIFReadWriter >> flushBuffer [
	bufStream isEmpty ifTrue: [ ^ self ].
	self nextPut: bufStream size.
	self nextPutAll: bufStream contents.
	bufStream := (ByteArray new: 256) writeStream.
]

{ #category : #'private-encoding' }
NeoGIFReadWriter >> flushCode [
	self flushBits
]

{ #category : #accessing }
NeoGIFReadWriter >> form [
	"By default, answer with the first Form available in the
	ImageFrames collection. If there are not any frames, answer nil"
	frames ifNil: [ ^ nil ].
	frames ifEmpty: [ ^ nil ].
	^ frames first form.
]

{ #category : #accessing }
NeoGIFReadWriter >> forms [
	frames ifNil: [ ^ nil ].
	^ frames collect: [ :f | f form ].
]

{ #category : #accessing }
NeoGIFReadWriter >> frames [
	^ frames
]

{ #category : #accessing }
NeoGIFReadWriter >> frames: aCollectionOfImageFrames [
	"Set the receiver's underlying collection of
	ImageFrame objects. Used when attempting to write
	out GIF images"
	frames := aCollectionOfImageFrames
]

{ #category : #testing }
NeoGIFReadWriter >> isAnimated [
	frames ifNil: [ ^ false ].
	^ frames size > 1
]

{ #category : #accessing }
NeoGIFReadWriter >> loopCount: aNumber [
	"Set looping. This must be done before any image is written!"
	loopCount := aNumber
]

{ #category : #'private-bits access' }
NeoGIFReadWriter >> nextBits [
	| integer readBitCount shiftCount byte |
	integer := 0.
	remainBitCount = 0 
		ifTrue: 
			[ readBitCount := 8.
			shiftCount := 0 ]
		ifFalse: 
			[ readBitCount := remainBitCount.
			shiftCount := remainBitCount - 8 ].
	[ readBitCount < codeSize ] whileTrue: 
		[ byte := self nextByte.
		byte == nil ifTrue: [ ^ eoiCode ].
		integer := integer + (byte bitShift: shiftCount).
		shiftCount := shiftCount + 8.
		readBitCount := readBitCount + 8 ].
	(remainBitCount := readBitCount - codeSize) = 0 
		ifTrue: [ byte := self nextByte ]
		ifFalse: [ byte := self peekByte ].
	byte == nil ifTrue: [ ^ eoiCode ].
	^ integer + (byte bitShift: shiftCount) bitAnd: maxCode
]

{ #category : #'private-bits access' }
NeoGIFReadWriter >> nextBitsPut: anInteger [ 
	| integer writeBitCount shiftCount |
	shiftCount := 0.
	remainBitCount = 0 
		ifTrue: 
			[ writeBitCount := 8.
			integer := anInteger ]
		ifFalse: 
			[ writeBitCount := remainBitCount.
			integer := bufByte + (anInteger bitShift: 8 - remainBitCount) ].
	[ writeBitCount < codeSize ] whileTrue: 
		[ self nextBytePut: ((integer bitShift: shiftCount) bitAnd: 255).
		shiftCount := shiftCount - 8.
		writeBitCount := writeBitCount + 8 ].
	(remainBitCount := writeBitCount - codeSize) = 0 
		ifTrue: [ self nextBytePut: (integer bitShift: shiftCount) ]
		ifFalse: [ bufByte := integer bitShift: shiftCount ].
	^ anInteger
]

{ #category : #'private-packing' }
NeoGIFReadWriter >> nextByte [
	bufStream atEnd
		ifTrue:
			[self atEnd ifTrue: [^nil].
			self fillBuffer].
	^bufStream next
]

{ #category : #'private-packing' }
NeoGIFReadWriter >> nextBytePut: aByte [
	bufStream nextPut: aByte.
	bufStream size >= 254 ifTrue: [self flushBuffer]
]

{ #category : #accessing }
NeoGIFReadWriter >> nextImage [
	"This method ensures older compatibility with ImageReadWriter.
	We respond with the Form corresponding to the *first image* on
	the receiver's read byte stream"
	self
		readHeader;
		readBody.
	^ self form.
	
]

{ #category : #accessing }
NeoGIFReadWriter >> nextPutFrame: anAnimatedImageFrame [
	"Given the current settings, write the bytes onto the
	output stream for the given ImageFrame and its form"
	| aForm reduced tempForm tempFrame |
	
	aForm := anAnimatedImageFrame form copy.
	aForm unhibernate.
	aForm depth > 8 ifTrue:[
		reduced := aForm colorReduced.  "minimize depth"
		reduced depth > 8 ifTrue: [
			"Not enough color space; do it the hard way."
			reduced := reduced asFormOfDepth: 8].
	] ifFalse:[reduced := aForm].
	reduced depth < 8 ifTrue: [
		"writeBitData: expects depth of 8"
		tempForm := reduced class extent: reduced extent depth: 8.
		(reduced isColorForm) ifTrue:[
			tempForm
				copyBits: reduced boundingBox
				from: reduced at: 0@0
				clippingBox: reduced boundingBox
				rule: Form over
				fillColor: nil
				map: nil.
			tempForm colors: reduced colors.
		] ifFalse: [reduced displayOn: tempForm].
		reduced := tempForm.
	].
	(reduced isColorForm) ifTrue:[
		(reduced colorsUsed includes: Color transparent) ifTrue: [
			transparentIndex := (reduced colors indexOf: Color transparent) - 1.
		]
	] ifFalse: [transparentIndex := nil].
	width := reduced width.
	height := reduced height.
	bitsPerPixel := reduced depth.
	colorPalette := reduced colormapIfNeededForDepth: 32.
	interlace := false.
	tempFrame := AnimatedImageFrame new 
		form: reduced;
		offset: anAnimatedImageFrame offset;
		delay: anAnimatedImageFrame delay;
		disposal: anAnimatedImageFrame disposal.
	self writeHeader.
	self writeFrameHeader: tempFrame.
	self writeBitData: reduced bits.
]

{ #category : #accessing }
NeoGIFReadWriter >> nextPutImage: aForm [
	"Given the current settings, write the bytes onto the
	output stream for the given ImageFrame and its form"
	| reduced tempForm tempFrame |
	
	aForm unhibernate.
	aForm depth > 8 ifTrue:[
		reduced := aForm colorReduced.  "minimize depth"
		reduced depth > 8 ifTrue: [
			"Not enough color space; do it the hard way."
			reduced := reduced asFormOfDepth: 8].
	] ifFalse:[reduced := aForm].
	reduced depth < 8 ifTrue: [
		"writeBitData: expects depth of 8"
		tempForm := reduced class extent: reduced extent depth: 8.
		(reduced isColorForm) ifTrue:[
			tempForm
				copyBits: reduced boundingBox
				from: reduced at: 0@0
				clippingBox: reduced boundingBox
				rule: Form over
				fillColor: nil
				map: nil.
			tempForm colors: reduced colors.
		] ifFalse: [reduced displayOn: tempForm].
		reduced := tempForm.
	].
	(reduced isColorForm) ifTrue:[
		(reduced colorsUsed includes: Color transparent) ifTrue: [
			transparentIndex := (reduced colors indexOf: Color transparent) - 1.
		]
	] ifFalse: [transparentIndex := nil].
	width := reduced width.
	height := reduced height.
	bitsPerPixel := reduced depth.
	colorPalette := reduced colormapIfNeededForDepth: 32.
	interlace := false.
	tempFrame := AnimatedImageFrame new 
		form: reduced;
		offset: reduced offset.
	self writeHeader.
	self writeFrameHeader: tempFrame.
	self writeBitData: reduced bits.
]

{ #category : #'private-packing' }
NeoGIFReadWriter >> peekByte [
	bufStream atEnd
		ifTrue:
			[self atEnd ifTrue: [^nil].
			self fillBuffer].
	^bufStream peek
]

{ #category : #'as yet unclassified' }
NeoGIFReadWriter >> processColorsFor: anImageFrame [
	"Colors can only be mapped after the GCE has been evaluated
	for a given image frame. We perform this action using either
	the local or global color table for this frame's form"
	| colorTable |
	colorTable := localColorTable ifNil: [ colorPalette ].
	
	"Use a copy so we don't mess up the global color table as we parse"
	colorTable := colorTable copyFrom: 1 to: colorTable size.
	
	transparentIndex 
		ifNotNil: [ 
			transparentIndex + 1 > colorTable size
				ifTrue: [ 
					anImageFrame transparentColor: Color white.
					colorTable := colorTable
										forceTo: transparentIndex + 1
										paddingWith: Color white ].
				"anImageFrame transparentColor: (colorTable at: transparentIndex + 1)."
				colorTable
					at: transparentIndex + 1
					put: Color transparent ].
	anImageFrame form colors: colorTable.
]

{ #category : #'private-decoding' }
NeoGIFReadWriter >> readApplicationExtension [
	"Uses the underlying stream to read a so-called
	Application Extension to the GIF Image. These extensions
	are at the whole file -- not individual frame like a GCE --
	level. It appears the only kind widely used is the NETSCAPE
	extension for determining the number of times an animated
	GIF should loop."
	| bytesFollow appName appAuthCode caughtInfo numSubBlocks loopVal1 loopVal2 |
	"How many bytes before data begins?
	Usually 11"
	bytesFollow := self next.
	appName := (String streamContents: [ :s |
		1 to: 8 do: [ :num |
			s
				nextPut: self next asCharacter ] ]).
	appAuthCode := (String streamContents: [ :s |
		1 to: 3 do: [ :num |
			s
				nextPut: self next asCharacter ] ]).
	caughtInfo := (appName size + appAuthCode size).
	caughtInfo = bytesFollow ifFalse: [ 
		(bytesFollow = caughtInfo) timesRepeat: [ 
			self next ] ].
	numSubBlocks := self next.
	appName = 'NETSCAPE' 
		ifTrue: [ 
			self next. "Data sub-block index (always 1)"
			"If it's the NETSCAPE extension, the next
			byte will set the loopCount. This is stored in
			a 2-byte lo-hi unsigned format"
			loopVal1 := self next.
			loopVal2 := self next.
			loopCount := (loopVal2 * 256) + loopVal1.
			self next = 0 ifFalse: [ ^ self error: 'Corrupt NETSCAPE Application Block' ].
			^ self ].

	"For now we ignore Application Extensions
	that are not the NETSCAPE kind"
	[ numSubBlocks = 0 ] whileFalse: [ 
		self next: numSubBlocks.
		numSubBlocks := self next ].
	
]

{ #category : #'private-decoding' }
NeoGIFReadWriter >> readBitData [
	"using modified Lempel-Ziv Welch algorithm."
	| outCodes outCount bitMask initCodeSize code curCode oldCode inCode finChar i bytes f c packedBits hasLocalColor localColorSize maxOutCodes |
	maxOutCodes := 4096.
	offset := self readWord @ self readWord.	"Image Left@Image Top"
	width := self readWord.
	height := self readWord.

	"---
	Local Color Table Flag        1 Bit
	Interlace Flag                1 Bit
	Sort Flag                     1 Bit
	Reserved                      2 Bits
	Size of Local Color Table     3 Bits
	----"
	packedBits := self next.
	interlace := (packedBits bitAnd: 64) ~= 0.
	hasLocalColor := (packedBits bitAnd: 128) ~= 0.
	localColorSize := 1 bitShift: (packedBits bitAnd: 7) + 1.
	hasLocalColor ifTrue: [ localColorTable := self readColorTable: localColorSize ].
	pass := 0.
	xpos := 0.
	ypos := 0.
	rowByteSize := (width + 3) // 4 * 4.
	remainBitCount := 0.
	bufByte := 0.
	bufStream := ByteArray new readStream.
	outCodes := ByteArray new: maxOutCodes + 1.
	outCount := 0.
	prefixTable := Array new: 4096.
	suffixTable := Array new: 4096.
	initCodeSize := self next.
	bitMask := (1 bitShift: initCodeSize) - 1.
	self setParameters: initCodeSize.
	bitsPerPixel > 8 ifTrue: [ ^ self error: 'never heard of a GIF that deep' ].
	bytes := ByteArray new: rowByteSize * height.
	[ (code := self readCode) = eoiCode ] whileFalse: 
		[ code = clearCode 
			ifTrue: 
				[ self setParameters: initCodeSize.
				curCode := oldCode := code := self readCode.
				finChar := curCode bitAnd: bitMask.
				"Horrible hack to avoid running off the end of the bitmap.  Seems to cure problem reading some gifs!?"
				xpos = 0 
					ifTrue: 
						[ ypos < height ifTrue: 
							[ bytes 
								at: ypos * rowByteSize + xpos + 1
								put: finChar ] ]
					ifFalse: 
						[ bytes 
							at: ypos * rowByteSize + xpos + 1
							put: finChar ].
				self updatePixelPosition ]
			ifFalse: 
				[ curCode := inCode := code.
				curCode >= freeCode ifTrue: 
					[ curCode := oldCode.
					outCodes 
						at: (outCount := outCount + 1)
						put: finChar ].
				[ curCode > bitMask ] whileTrue: 
					[ outCount > maxOutCodes ifTrue: [ ^ self error: 'corrupt GIF file (OutCount)' ].
					outCodes 
						at: (outCount := outCount + 1)
						put: (suffixTable at: curCode + 1).
					curCode := prefixTable at: curCode + 1 ].
				finChar := curCode bitAnd: bitMask.
				outCodes 
					at: (outCount := outCount + 1)
					put: finChar.
				i := outCount.
				[ i > 0 ] whileTrue: [
					bytes 
						at: ypos * rowByteSize + xpos + 1
						put: (outCodes at: i).
					self updatePixelPosition.
					i := i - 1 ].
				outCount := 0.
				prefixTable 
					at: freeCode + 1
					put: oldCode.
				suffixTable 
					at: freeCode + 1
					put: finChar.
				oldCode := inCode.
				freeCode := freeCode + 1.
				self checkCodeSize ] ].
	"prefixTable := suffixTable := nil."
	"WARNING: Change the above to get working again"
	f := ColorForm 
		extent: width @ height
		depth: 8.
	f bits copyFromByteArray: bytes.
	self halt.
	"Squeak can handle depths 1, 2, 4, and 8"
	bitsPerPixel > 4 ifTrue: [ ^ f ].
	"reduce depth to save space"
	"c := ColorForm 
		extent: width @ height
		depth: (bitsPerPixel = 3 
				ifTrue: [ 4 ]
				ifFalse: [ bitsPerPixel ])."
	c := ColorForm
		extent: width@height
		depth: bitsPerPixel.
	f displayOn: c.
	^ c
]

{ #category : #'private-decoding' }
NeoGIFReadWriter >> readBody [
	"Read the GIF blocks. Modified to return a frame."
	| form extype block blocksize packedFields delay1 disposal frame |
	frame := nil.
	frames := OrderedCollection new.
	[ stream atEnd ] whileFalse: [ 
		block := self next.
		
		"If we have reached the terminator byte, return."
		block = Terminator ifTrue: [ ^ frame ].
		block = ImageSeparator 
			ifTrue: [ 
				frame ifNil: [ frame := AnimatedImageFrame new ].
				frame form: (self altReadBitDataOnFrame: frame). "Adjusting message for testing"
				frame offset: offset. "Set from instance var, which is set in readBitData"
				
				frames add: frame.
				self processColorsFor: frame.
				self next = Terminator ifTrue: [ ^ frames last ].
				frame := nil. ]
			ifFalse: 
				[ "If it's not actual image data, perhaps
					it's an Extension of some kind (there can be several)"
					block = Extension 
						ifTrue: [ 
							frame ifNil: [ frame := AnimatedImageFrame new ].
							self readExtensionBlock: block withFrame: frame ]
						ifFalse: [ ^ self error: 'Unknown Bytes!' ] ] 
		].
	^ frames.
]

{ #category : #'private-decoding' }
NeoGIFReadWriter >> readCode [
	^self nextBits
]

{ #category : #'private-decoding' }
NeoGIFReadWriter >> readColorTable: numberOfEntries [ 
	| array r g b |
	array := Array new: numberOfEntries.
	1 
		to: array size
		do: 
			[ :i | 
			r := self next.
			g := self next.
			b := self next.
			array 
				at: i
				put: (Color 
						r: r
						g: g
						b: b
						range: 255) ].
	^ array
]

{ #category : #'as yet unclassified' }
NeoGIFReadWriter >> readDisposal: aPackedByte [
	"Read the three-bit disposal flag from
	the packed byte in the Graphic Control Extension block.
	Disposal is three-bits with the following codes:
	 |0 0 0 [0 0 0] 0 0|
	1 => leave current frame and draw on top of it (#leaveCurrent)
	2 => Restore to background color (#restoreBackground)
	3 => Restore to state before current frame was drawn (#restorePrevState)"
	| least middle both |
	(both := (aPackedByte bitAnd: 12) = 12).
	both ifTrue: [ ^ #restorePrevState ].
	
	least := (aPackedByte bitAnd: 4) = 4.
	least ifTrue: [ ^ #leaveCurrent ].
	
	middle := (aPackedByte bitAnd: 8) = 8.
	middle ifTrue: [ ^ #restoreBackground ].
	
	^ #otherDisposal
	
]

{ #category : #'private-decoding' }
NeoGIFReadWriter >> readExtensionBlock: aGifBlock withFrame: anImageFrame [
	"Determine which type of extension block we are
	looking at. The most common is the Graphic Control Extension (GCE)
	which tells us information about the image frame, including delays
	offsets in the canvas, and how to dispose of the frame in animation"
	| extensionType packedByte delayByte1 delayByte2 |
	extensionType := self next.
	
	"255 is an Application Extension.
	 This seems to always be the NETSCAPE
	 extension, which has looping information.
	This extension does not affect individual frames,
	but rather sets the loopCount for the whole image"
	extensionType = 255 ifTrue: [ 
		^ self readApplicationExtension ].
	
	
	"249 Corresponds to the GCE"
	extensionType = 249 ifTrue: [ 
		self next = 4 ifFalse: [ ^ self "The GIF is likely corrupt in this case" ].
		"====
		Reserved                      3 Bits (Ignore)
		Disposal Method               3 Bits 
		User Input Flag               1 Bit  (Ignore)
		Transparent Color Flag        1 Bit  (Need to Implement)
		==="
		packedByte := self next.
		delayByte1 := self next.
		delayByte2 := self next.
		transparentIndex := self next.
		(packedByte bitAnd: 1) = 0 "Changed to see if other endian is the real end..."
			ifTrue: [ transparentIndex := nil ].
		anImageFrame 
			disposal: (self readDisposal: packedByte);
			"Delay time is stored as 2 bytes unsigned"
			delay: (delayByte2 * 256 + delayByte1) * 10.
		self next = 0 ifFalse: [ ^ self error: 'Corrupt GCE Block!' ].
		^ self ].
	
	"If you get to this point, we don't know the Extension Type"
	^ self error: 'Unknown GIF Extension: ',(extensionType asString).
]

{ #category : #'private-decoding' }
NeoGIFReadWriter >> readHeader [
	| is89 byte hasColorMap |
	(self hasMagicNumber: 'GIF87a' asByteArray) 
		ifTrue: [ is89 := false ]
		ifFalse: 
			[ (self hasMagicNumber: 'GIF89a' asByteArray) 
				ifTrue: [ is89 := true ]
				ifFalse: [ ^ self error: 'This does not appear to be a GIF file' ] ].
	"Width and Height for whole canvas, not
	just an invididual frame/form"
	canvasWidth := self readWord.
	canvasHeight := self readWord.
	byte := self next.
	hasColorMap := (byte bitAnd: 128) ~= 0.
	bitsPerPixel := (byte bitAnd: 7) + 1.
	backgroundColorIndex := self next.
	self next ~= 0 ifTrue: 
		[ is89 ifFalse: [ ^ self error: 'corrupt GIF file (screen descriptor)' ] ].
	hasColorMap 
		ifTrue: [ colorPalette := self readColorTable: (1 bitShift: bitsPerPixel) ]
		ifFalse: 
			[ colorPalette := nil	"Palette monochromeDefault" ]
]

{ #category : #'private-encoding' }
NeoGIFReadWriter >> readPixelFrom: bits [ 
	"Since bits is a Bitmap with 32 bit values, watch out for the
padding at the end of each row.  But, GIF format already wants padding to
32 bit boundary!  OK as is.  tk 9/14/97"
	| pixel |
	ypos >= height ifTrue: [ ^ nil ].
	pixel := bits byteAt: ypos * rowByteSize + xpos + 1.
	self updatePixelPosition.
	^ pixel
]

{ #category : #'private-decoding' }
NeoGIFReadWriter >> readWord [
	^self next + (self next bitShift: 8)
]

{ #category : #private }
NeoGIFReadWriter >> setParameters: initCodeSize [ 
	clearCode := 1 bitShift: initCodeSize.
	eoiCode := clearCode + 1.
	freeCode := clearCode + 2.
	codeSize := initCodeSize + 1.
	maxCode := (1 bitShift: codeSize) - 1
]

{ #category : #accessing }
NeoGIFReadWriter >> setStream: aStream [ 
	"Feed it in from an existing source"
	stream := aStream
]

{ #category : #'private-decoding' }
NeoGIFReadWriter >> skipBitData [
	| misc blocksize |
	self readWord.	"skip Image Left"
	self readWord.	"skip Image Top"
	self readWord.	"width"
	self readWord.	"height"
	misc := self next.
	(misc bitAnd: 128) = 0 ifFalse: 
		[ "skip colormap"
		1 
			to: (1 bitShift: (misc bitAnd: 7) + 1)
			do: 
				[ :i | 
				self
					next;
					next;
					next ] ].
	self next.	"minimum code size"
	[ (blocksize := self next) > 0 ] whileTrue: [ self next: blocksize ]
]

{ #category : #accessing }
NeoGIFReadWriter >> understandsImageFormat [
	^('abc' collect: [:x | stream next asCharacter]) = 'GIF'
]

{ #category : #private }
NeoGIFReadWriter >> updatePixelPosition [
	(xpos := xpos + 1) >= width ifFalse: [ ^ self ].
	xpos := 0.
	interlace ifFalse: 
		[ ypos := ypos + 1.
		^ self ].
	pass = 0 ifTrue: 
		[ (ypos := ypos + 8) >= height ifTrue: 
			[ pass := pass + 1.
			ypos := 4 ].
		^ self ].
	pass = 1 ifTrue: 
		[ (ypos := ypos + 8) >= height ifTrue: 
			[ pass := pass + 1.
			ypos := 2 ].
		^ self ].
	pass = 2 ifTrue: 
		[ (ypos := ypos + 4) >= height ifTrue: 
			[ pass := pass + 1.
			ypos := 1 ].
		^ self ].
	pass = 3 ifTrue: 
		[ ypos := ypos + 2.
		^ self ].
	^ self error: 'can''t happen'
]

{ #category : #'private-encoding' }
NeoGIFReadWriter >> writeBitData: bits [ 
	"using modified Lempel-Ziv Welch algorithm."
	| encoder initCodeSize |
	encoder := NeoLzwGifEncoder new
		rowByteSize: (width * 8 + 31) // 32 * 4;
		extent: width@height;
		codeStream: stream.
	initCodeSize := bitsPerPixel <= 1 
		ifTrue: [ 2 ]
		ifFalse: [ bitsPerPixel ].
	encoder minimumCodeSize: initCodeSize.
	encoder encode: bits.
]

{ #category : #'private-encoding' }
NeoGIFReadWriter >> writeCode: aCode [
	self nextBitsPut: aCode
]

{ #category : #'private-encoding' }
NeoGIFReadWriter >> writeCodeAndCheckCodeSize: aCode [
	self writeCode: aCode.
	self checkCodeSize
]

{ #category : #accessing }
NeoGIFReadWriter >> writeFrameHeader: anImageFrame [
	"Write any Extensions and/or headers that apply
	to individual frames/subimages"
	| interlaceByte |
	anImageFrame delay notNil | transparentIndex notNil ifTrue: [ 
		self writeGCEForFrame: anImageFrame ].
	
	"Next is the image descriptor"
	self 
		nextPut: ImageSeparator;
		writeWord: (anImageFrame offset x);
		writeWord: (anImageFrame offset y);
		writeWord: (anImageFrame form extent x);
		writeWord: (anImageFrame form extent y).
	
	interlaceByte := interlace
		ifTrue: [ 64 ]
		ifFalse: [ 0 ].
	self nextPut: interlaceByte
	
]

{ #category : #'private-encoding' }
NeoGIFReadWriter >> writeGCEForFrame: anAnimatedImageFrame [
	"Writes a Graphics Control Extension onto
	the output stream for the given image frame"
	| nextDelay |
	nextDelay := anAnimatedImageFrame delay.
	anAnimatedImageFrame delay ifNil: [ nextDelay := 0 ].
	self 
		nextPut: Extension;
		nextPutAll: #(249 4) asByteArray;
		nextPut: (transparentIndex
				ifNil: [ 0 ]
				ifNotNil: [ 9 ]); "I think this is where the problem is!! TODO"
		writeWord: nextDelay / 10;
		nextPut: (transparentIndex ifNil: [ 0 ]);
		nextPut: 0.
]

{ #category : #'private-encoding' }
NeoGIFReadWriter >> writeHeader [
	| byte |
	"Write the overall image file header onto the
	output stream. This includes the global information
	about the file, such as canvasWidth etc. Only do so
	if the stream is in the initial position."
	stream position = 0 ifFalse: [ ^ self ].

	self nextPutAll: 'GIF89a' asByteArray.
	self writeWord: width.	"Screen Width"
	self writeWord: height.	"Screen Height"
	byte := 128.	"has color map"
	byte := byte bitOr: (bitsPerPixel - 1 bitShift: 5).	"color resolution"
	byte := byte bitOr: bitsPerPixel - 1.	"bits per pixel"
	self nextPut: byte.
	self nextPut: 0.	"background color."
	self nextPut: 0.	"reserved / unused 'pixel aspect ratio"
	colorPalette do: 
		[ :pixelValue | 
		self
			nextPut: ((pixelValue bitShift: -16) bitAnd: 255);
			nextPut: ((pixelValue bitShift: -8) bitAnd: 255);
			nextPut: (pixelValue bitAnd: 255) ].
	loopCount notNil ifTrue: 
		[ self writeNetscapeExtension ].
]

{ #category : #'private-encoding' }
NeoGIFReadWriter >> writeNetscapeExtension [
	"Writes a GIF Application Extension corresponding
	to the NETSCAPE2.0 version, with specifies the loopCount."
	self
		nextPut: Extension;
		nextPut: 255; "Indicates Application Extension"
		nextPut: 11; "Indicates how many bytes follow, almost always 11"
		nextPutAll: ('NETSCAPE2.0' asByteArray);
		nextPut: 3;
		nextPut: 1;
		writeWord: (loopCount ifNil: [ 0 ]);
		nextPut: 0.
]

{ #category : #'private-encoding' }
NeoGIFReadWriter >> writeWord: aWord [
	self nextPut: (aWord bitAnd: 255).
	self nextPut: ((aWord bitShift: -8) bitAnd: 255).
	^aWord
]
