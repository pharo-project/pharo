"
I am GIFReadWriter.
I am a concrete ImageReadWriter.

Updated implementation of a GIF file (byte-level) decoder.

I implment a Stream-like behavior over a GIF image file, and can both read and write GIF files.

Previously, two classes distinguished between ""still"" and ""animated"" GIFs. However, the standard specifies that any GIF can have ""frames"" and be animated. This reimplementation treats this as normal.

See these links for more detailed information:
 
 https://www.w3.org/Graphics/GIF/spec-gif89a.txt
 https://en.wikipedia.org/wiki/GIF
 http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp

For writing GIF files, I take a collection of AnimatedImageFrame objects and write the appropriate headers, Graphics Control Extensions, and everything else needed for writing an animated GIF.

For reading GIF files, I take a binary filestream and set my own `frames` variable to be a collection of AnimatedImageFrames, which themselves contain decoded Forms and instructions for disposal, delay, etc.

NOTE: I make use of the LzwGifDecoder and LzwGifEncoder classes in order to encode/decode individual bitmap data for each image frame of the GIF.

See `GIFReadWriter exampleAnim` for more information.  
"
Class {
	#name : #GIFReadWriter,
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
		'transparentIndex',
		'localColorTable',
		'loopCount',
		'offset',
		'frames',
		'canvasWidth',
		'canvasHeight',
		'backgroundColorIndex',
		'comment'
	],
	#classVars : [
		'Extension',
		'ImageSeparator',
		'Terminator'
	],
	#category : #'Graphics-Files'
}

{ #category : #examples }
GIFReadWriter class >> exampleAnim [
	"This example writes out an animated gif of
	 a red circle"

	| writer extent center frameDelay |
	writer := GIFReadWriter on: (File openForWriteFileNamed: 'anim.gif').
	writer loopCount: 20.		"Repeat 20 times"
	frameDelay := 10.		"Wait 10/100 seconds"
	extent := 42@42.
	center := extent / 2.
	Cursor write showWhile: [
		[2 to: center x - 1 by: 2 do: [:r |
			"Make a fancy anim without using Canvas - inefficient as hell"
			| frame |
			frame := AnimatedImageFrame new
				delay: frameDelay;
				form: (ColorForm extent: extent depth: 8).
			0.0 to: 359.0 do: [:theta | frame form colorAt: (center + (Point r: r degrees: theta)) rounded put: Color red].
			writer nextPutFrame: frame]
		]	ensure: [writer close]].
]

{ #category : #examples }
GIFReadWriter class >> grabScreenAndSaveOnDisk [
	"GIFReadWriter grabScreenAndSaveOnDisk"
	| form fileName |
	
	form := Form fromUser.
	form bits size = 0 ifTrue: [ ^ self inform: 'Empty region selected.' ].
	
	fileName := (FileSystem disk workingDirectory / 'Pharo', 'gif') fullName.
		
	UIManager default 
		informUser: 'Writing ' translated, fileName
		during: 
			[ GIFReadWriter 
				putForm: form
				onFileNamed: fileName ]
]

{ #category : #initialization }
GIFReadWriter class >> initialize [
	"GIFReadWriter initialize"
	ImageSeparator := $, asInteger.
	Extension := $! asInteger.
	Terminator := $; asInteger
]

{ #category : #'image reading/writing' }
GIFReadWriter class >> typicalFileExtensions [
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
GIFReadWriter class >> wantsToHandleGIFs [
	^ true
]

{ #category : #accessing }
GIFReadWriter >> backgroundColor [
	backgroundColorIndex ifNotNil: [ 
		colorPalette ifNotNil: [ 
			^ colorPalette at: backgroundColorIndex + 1]].
	^ Color transparent.
]

{ #category : #accessing }
GIFReadWriter >> canvasHeight [
	^ canvasHeight
]

{ #category : #accessing }
GIFReadWriter >> canvasHeight: aNumber [
	canvasHeight := aNumber
]

{ #category : #accessing }
GIFReadWriter >> canvasWidth [
	^ canvasWidth
]

{ #category : #accessing }
GIFReadWriter >> canvasWidth: aNumber [
	canvasWidth := aNumber
]

{ #category : #'stream access' }
GIFReadWriter >> close [
	"Write terminator"
	self nextPut: Terminator.
	^super close
]

{ #category : #accessing }
GIFReadWriter >> delay [
	^ self
		deprecated: 'Delay values for animaed GIFs are no longer set by the GIFReadWriter. Set them in each AnimatedImageFrame instead! See GIFReaderWriter class >> #exampleAnim for an example.'
]

{ #category : #accessing }
GIFReadWriter >> delay: anInteger [
	^ self
		deprecated: 'Delay values for animaed GIFs are no longer set by the GIFReadWriter. Set them in each AnimatedImageFrame instead! See GIFReaderWriter class >> #exampleAnim for an example.'
]

{ #category : #accessing }
GIFReadWriter >> form [
	"By default, answer with the first Form available in the
	ImageFrames collection. If there are not any frames, answer nil"
	frames ifNil: [ ^ nil ].
	frames ifEmpty: [ ^ nil ].
	^ frames first form.
]

{ #category : #accessing }
GIFReadWriter >> forms [
	frames ifNil: [ ^ nil ].
	^ frames collect: [ :f | f form ].
]

{ #category : #accessing }
GIFReadWriter >> frames [
	^ frames
]

{ #category : #accessing }
GIFReadWriter >> frames: aCollectionOfImageFrames [
	"Set the receiver's underlying collection of
	ImageFrame objects. Used when attempting to write
	out GIF images"
	frames := aCollectionOfImageFrames
]

{ #category : #testing }
GIFReadWriter >> isAnimated [
	frames ifNil: [ ^ false ].
	^ frames size > 1
]

{ #category : #accessing }
GIFReadWriter >> loopCount: aNumber [
	"Set looping. This must be done before any image is written!"
	loopCount := aNumber
]

{ #category : #accessing }
GIFReadWriter >> nextImage [
	"This method ensures older compatibility with ImageReadWriter.
	We respond with the Form corresponding to the *first image* on
	the receiver's read byte stream"
	self
		readHeader;
		readBody.
	^ self form.
	
]

{ #category : #accessing }
GIFReadWriter >> nextPutFrame: anAnimatedImageFrame [
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
GIFReadWriter >> nextPutImage: aForm [
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

{ #category : #'private-decoding' }
GIFReadWriter >> processColorsFor: anImageFrame [
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
					colorTable := colorTable
										forceTo: transparentIndex + 1
										paddingWith: Color white ].
				colorTable
					at: transparentIndex + 1
					put: Color transparent ].
	anImageFrame form colors: colorTable.
]

{ #category : #'private-decoding' }
GIFReadWriter >> readApplicationExtension [
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
GIFReadWriter >> readBitDataOnFrame: aFrame [
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

	decoder := LzwGifDecoder new.
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

{ #category : #'private-decoding' }
GIFReadWriter >> readBody [
	"Read the GIF blocks. Modified to return a frame."
	| block frame |
	frame := nil.
	frames := OrderedCollection new.
	[ stream atEnd ] whileFalse: [ 
		block := self next.
		
		"If we have reached the terminator byte, return."
		block = Terminator ifTrue: [ ^ frame ].
		block = ImageSeparator 
			ifTrue: [ 
				frame ifNil: [ frame := AnimatedImageFrame new ].
				frame form: (self readBitDataOnFrame: frame). "Adjusting message for testing"
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
GIFReadWriter >> readColorTable: numberOfEntries [ 
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

{ #category : #'private-decoding' }
GIFReadWriter >> readCommentExtension [
	| blockTerminator |
	blockTerminator := self next.
	blockTerminator > 0
		ifTrue: [ comment := self next: blockTerminator.
			blockTerminator := self next ].
	blockTerminator = 0
		ifFalse: [ ^ self error: 'Invalid Block Terminator' ]
]

{ #category : #'private-decoding' }
GIFReadWriter >> readDisposal: aPackedByte [
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
GIFReadWriter >> readExtensionBlock: aGifBlock withFrame: anImageFrame [
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

	extensionType = 254 ifTrue: [ 
		^ self readCommentExtension ].

	"If you get to this point, we don't know the Extension Type"
	^ self error: 'Unknown GIF Extension: ',(extensionType asString).
]

{ #category : #'private-decoding' }
GIFReadWriter >> readHeader [
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

{ #category : #'private-decoding' }
GIFReadWriter >> readWord [
	^self next + (self next bitShift: 8)
]

{ #category : #accessing }
GIFReadWriter >> setStream: aStream [ 
	"Feed it in from an existing source"
	stream := aStream
]

{ #category : #accessing }
GIFReadWriter >> understandsImageFormat [
	^('abc' collect: [:x | stream next asCharacter]) = 'GIF'
]

{ #category : #private }
GIFReadWriter >> updatePixelPosition [
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
GIFReadWriter >> writeBitData: bits [ 
	"using modified Lempel-Ziv Welch algorithm."
	| encoder initCodeSize |
	encoder := LzwGifEncoder new
		rowByteSize: (width * 8 + 31) // 32 * 4;
		extent: width@height;
		codeStream: stream.
	initCodeSize := bitsPerPixel <= 1 
		ifTrue: [ 2 ]
		ifFalse: [ bitsPerPixel ].
	encoder minimumCodeSize: initCodeSize.
	encoder encode: bits.
]

{ #category : #writing }
GIFReadWriter >> writeDisposal: aSymbol toPackedByte: aByte [
	"Using the GIF Graphics Control Extension
	packed byte format, respond with a modified version
	of the passed byte that includes the correct 3-bit
	disposal code corresponding to the passed in symbol"
	
	aSymbol = #restoreBackground
		ifTrue: [ 
			"This is a value of 2 in the 3-bit structure,
			so 010, then shifted two to the left (equal to 8)"
			^ aByte + (2 bitShift: 2) ].
	
	aSymbol = #leaveCurrent
		ifTrue: [ 
			"This is a value of 1 in the 3-bit structure,
			so 001, then shifted two to the left (equal to 4)"
			^ aByte + (1 bitShift: 2) ].
	
	aSymbol = #restorePrevState
		ifTrue: [ 
			"This is a value of 3 in the 3-bit structure,
			so 011, then shifted two to the left (equal to 12)"
			^ aByte + (3 bitShift: 2) ].
	^ aByte
		
]

{ #category : #accessing }
GIFReadWriter >> writeFrameHeader: anImageFrame [
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
GIFReadWriter >> writeGCEForFrame: anAnimatedImageFrame [
	"Writes a Graphics Control Extension onto
	the output stream for the given image frame"
	| nextDelay packedByte |
	nextDelay := anAnimatedImageFrame delay.
	anAnimatedImageFrame delay ifNil: [ nextDelay := 0 ].
	"Set the bits of the packed byte"
	"====
		Reserved                      3 Bits (Ignore)
		Disposal Method               3 Bits 
		User Input Flag               1 Bit  (Ignore)
		Transparent Color Flag        1 Bit 
		==="
	packedByte := 0.
	transparentIndex
		ifNotNil: [ packedByte := 1 ].
	packedByte := self 
		writeDisposal: (anAnimatedImageFrame disposal)
		toPackedByte: packedByte.
	
	self 
		nextPut: Extension;
		nextPutAll: #(249 4) asByteArray;
		nextPut: packedByte;
		"nextPut: (transparentIndex
				ifNil: [ 0 ]
				ifNotNil: [ 9 ]);"
		writeWord: nextDelay // 10;
		nextPut: (transparentIndex ifNil: [ 0 ]);
		nextPut: 0.
]

{ #category : #'private-encoding' }
GIFReadWriter >> writeHeader [
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
GIFReadWriter >> writeNetscapeExtension [
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
GIFReadWriter >> writeWord: aWord [
	self nextPut: (aWord bitAnd: 255).
	self nextPut: ((aWord bitShift: -8) bitAnd: 255).
	^aWord
]
