"
Copyright (c) Kazuki Yasumatsu, 1995. All rights reserved.

Used with permission.  Modified for use in Squeak.
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
		'delay',
		'loopCount',
		'offset'
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
	"GIFReadWriter exampleAnim"

	| writer extent center |
	writer := GIFReadWriter on: (FileStream newFileNamed: 'anim.gif').
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
	^ false
]

{ #category : #private }
GIFReadWriter >> checkCodeSize [
	(freeCode > maxCode and: [ codeSize < 12 ]) ifTrue: 
		[ codeSize := codeSize + 1.
		maxCode := (1 bitShift: codeSize) - 1 ]
]

{ #category : #'stream access' }
GIFReadWriter >> close [
	"Write terminator"
	self nextPut: Terminator.
	^super close
]

{ #category : #accessing }
GIFReadWriter >> delay: aNumberOrNil [
	"Set delay for next image in hundredth (1/100) of seconds"
	delay := aNumberOrNil
]

{ #category : #'private-packing' }
GIFReadWriter >> fillBuffer [
	| packSize |
	packSize := self next.
	bufStream := (self next: packSize) readStream
]

{ #category : #'private-bits access' }
GIFReadWriter >> flushBits [
	remainBitCount = 0 ifFalse: 
		[ self nextBytePut: bufByte.
		remainBitCount := 0 ].
	self flushBuffer
]

{ #category : #'private-packing' }
GIFReadWriter >> flushBuffer [
	bufStream isEmpty ifTrue: [ ^ self ].
	self nextPut: bufStream size.
	self nextPutAll: bufStream contents.
	bufStream := (ByteArray new: 256) writeStream.
]

{ #category : #'private-encoding' }
GIFReadWriter >> flushCode [
	self flushBits
]

{ #category : #accessing }
GIFReadWriter >> loopCount: aNumber [
	"Set looping. This must be done before any image is written!"
	loopCount := aNumber
]

{ #category : #'private-bits access' }
GIFReadWriter >> nextBits [
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
GIFReadWriter >> nextBitsPut: anInteger [ 
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
GIFReadWriter >> nextByte [
	bufStream atEnd
		ifTrue:
			[self atEnd ifTrue: [^nil].
			self fillBuffer].
	^bufStream next
]

{ #category : #'private-packing' }
GIFReadWriter >> nextBytePut: aByte [
	bufStream nextPut: aByte.
	bufStream size >= 254 ifTrue: [self flushBuffer]
]

{ #category : #accessing }
GIFReadWriter >> nextImage [
	"Read in the next GIF image from the stream."
	| f thisImageColorTable |
	localColorTable := nil.
	self readHeader.
	f := self readBody.
	f ifNil: [ ^ self error: 'corrupt GIF file' ].
	thisImageColorTable := localColorTable ifNil: [ colorPalette ].
	transparentIndex ifNotNil: 
		[ transparentIndex + 1 > thisImageColorTable size ifTrue: 
			[ thisImageColorTable := thisImageColorTable 
				forceTo: transparentIndex + 1
				paddingWith: Color white ].
		thisImageColorTable 
			at: transparentIndex + 1
			put: Color transparent ].
	f colors: thisImageColorTable.
	^ f
]

{ #category : #accessing }
GIFReadWriter >> nextPutImage: aForm [

	| reduced tempForm |
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
	self writeHeader.
	self writeBitData: reduced bits.

]

{ #category : #'private-packing' }
GIFReadWriter >> peekByte [
	bufStream atEnd
		ifTrue:
			[self atEnd ifTrue: [^nil].
			self fillBuffer].
	^bufStream peek
]

{ #category : #'private-decoding' }
GIFReadWriter >> readBitData [
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
				[ i > 0 ] whileTrue: 
					[ "self writePixel: (outCodes at: i) to: bits"
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
	prefixTable := suffixTable := nil.
	f := ColorForm 
		extent: width @ height
		depth: 8.
	f bits copyFromByteArray: bytes.
	"Squeak can handle depths 1, 2, 4, and 8"
	bitsPerPixel > 4 ifTrue: [ ^ f ].
	"reduce depth to save space"
	c := ColorForm 
		extent: width @ height
		depth: (bitsPerPixel = 3 
				ifTrue: [ 4 ]
				ifFalse: [ bitsPerPixel ]).
	f displayOn: c.
	^ c
]

{ #category : #'private-decoding' }
GIFReadWriter >> readBody [
	"Read the GIF blocks. Modified to return a form.  "
	| form extype block blocksize packedFields delay1 |
	form := nil.
	[ stream atEnd ] whileFalse: 
		[ block := self next.
		block = Terminator ifTrue: [ ^ form ].
		block = ImageSeparator 
			ifTrue: 
				[ form
					ifNil: [ form := self readBitData ]
					ifNotNil: [ self skipBitData ] ]
			ifFalse: 
				[ block = Extension ifFalse: [ ^ form	"^ self error: 'Unknown block type'" ].
				"Extension block"
				extype := self next.	"extension type"
				extype = 249 
					ifTrue: 
						[ "graphics control"
						self next = 4 ifFalse: [ ^ form	"^ self error: 'corrupt GIF file'" ].
						"====
				Reserved                      3 Bits
				Disposal Method               3 Bits
				User Input Flag               1 Bit
				Transparent Color Flag        1 Bit
				==="
						packedFields := self next.
						delay1 := self next.	"delay time 1"
						delay := (self next * 256 + delay1) * 10.	"delay time 2"
						transparentIndex := self next.
						(packedFields bitAnd: 1) = 0 ifTrue: [ transparentIndex := nil ].
						self next = 0 ifFalse: [ ^ form	"^ self error: 'corrupt GIF file'" ] ]
					ifFalse: 
						[ "Skip blocks"
						[ (blocksize := self next) > 0 ] whileTrue: 
							[ "Read the block and ignore it and eat the block terminator"
							self next: blocksize ] ] ] ]
]

{ #category : #'private-decoding' }
GIFReadWriter >> readCode [
	^self nextBits
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
GIFReadWriter >> readHeader [
	| magic is89 byte hasColorMap |
	magic := stream next: 6.
	(magic = 'GIF87a' asByteArray) 
		ifTrue: [ is89 := false ]
		ifFalse: 
			[ (magic = 'GIF89a' asByteArray) 
				ifTrue: [ is89 := true ]
				ifFalse: [ ^ self error: 'This does not appear to be a GIF file' ] ].
	self readWord.	"skip Screen Width"
	self readWord.	"skip Screen Height"
	byte := self next.
	hasColorMap := (byte bitAnd: 128) ~= 0.
	bitsPerPixel := (byte bitAnd: 7) + 1.
	byte := self next.	"skip background color."
	self next ~= 0 ifTrue: 
		[ is89 ifFalse: [ ^ self error: 'corrupt GIF file (screen descriptor)' ] ].
	hasColorMap 
		ifTrue: [ colorPalette := self readColorTable: (1 bitShift: bitsPerPixel) ]
		ifFalse: 
			[ "Transcript cr; show: 'GIF file does not have a color map.'."
			colorPalette := nil	"Palette monochromeDefault" ]
]

{ #category : #'private-encoding' }
GIFReadWriter >> readPixelFrom: bits [ 
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
GIFReadWriter >> readWord [
	^self next + (self next bitShift: 8)
]

{ #category : #private }
GIFReadWriter >> setParameters: initCodeSize [ 
	clearCode := 1 bitShift: initCodeSize.
	eoiCode := clearCode + 1.
	freeCode := clearCode + 2.
	codeSize := initCodeSize + 1.
	maxCode := (1 bitShift: codeSize) - 1
]

{ #category : #accessing }
GIFReadWriter >> setStream: aStream [ 
	"Feed it in from an existing source"
	stream := aStream
]

{ #category : #'private-decoding' }
GIFReadWriter >> skipBitData [
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
	| maxBits maxMaxCode tSize initCodeSize ent tShift fCode pixel index disp nomatch |
	pass := 0.
	xpos := 0.
	ypos := 0.
	rowByteSize := (width * 8 + 31) // 32 * 4.
	remainBitCount := 0.
	bufByte := 0.
	bufStream := (ByteArray new: 256) writeStream.
	maxBits := 12.
	maxMaxCode := 1 bitShift: maxBits.
	tSize := 5003.
	prefixTable := Array new: tSize.
	suffixTable := Array new: tSize.
	initCodeSize := bitsPerPixel <= 1 
		ifTrue: [ 2 ]
		ifFalse: [ bitsPerPixel ].
	self nextPut: initCodeSize.
	self setParameters: initCodeSize.
	tShift := 0.
	fCode := tSize.
	[ fCode < 65536 ] whileTrue: 
		[ tShift := tShift + 1.
		fCode := fCode * 2 ].
	tShift := 8 - tShift.
	1 
		to: tSize
		do: 
			[ :i | 
			suffixTable 
				at: i
				put: -1 ].
	self writeCodeAndCheckCodeSize: clearCode.
	ent := self readPixelFrom: bits.
	[ (pixel := self readPixelFrom: bits) == nil ] whileFalse: 
		[ fCode := (pixel bitShift: maxBits) + ent.
		index := ((pixel bitShift: tShift) bitXor: ent) + 1.
		(suffixTable at: index) = fCode 
			ifTrue: [ ent := prefixTable at: index ]
			ifFalse: 
				[ nomatch := true.
				(suffixTable at: index) >= 0 ifTrue: 
					[ disp := tSize - index + 1.
					index = 1 ifTrue: [ disp := 1 ].
					"probe"
					
					[ (index := index - disp) < 1 ifTrue: [ index := index + tSize ].
					(suffixTable at: index) = fCode ifTrue: 
						[ ent := prefixTable at: index.
						nomatch := false
						"continue whileFalse:" ].
					nomatch and: [ (suffixTable at: index) > 0 ] ] whileTrue: 
						[ "probe"
						 ] ].
				"nomatch"
				nomatch ifTrue: 
					[ self writeCodeAndCheckCodeSize: ent.
					ent := pixel.
					freeCode < maxMaxCode 
						ifTrue: 
							[ prefixTable 
								at: index
								put: freeCode.
							suffixTable 
								at: index
								put: fCode.
							freeCode := freeCode + 1 ]
						ifFalse: 
							[ self writeCodeAndCheckCodeSize: clearCode.
							1 
								to: tSize
								do: 
									[ :i | 
									suffixTable 
										at: i
										put: -1 ].
							self setParameters: initCodeSize ] ] ] ].
	prefixTable := suffixTable := nil.
	self writeCodeAndCheckCodeSize: ent.
	self writeCodeAndCheckCodeSize: eoiCode.
	self flushCode.
	self nextPut: 0	"zero-length packet"
]

{ #category : #'private-encoding' }
GIFReadWriter >> writeCode: aCode [
	self nextBitsPut: aCode
]

{ #category : #'private-encoding' }
GIFReadWriter >> writeCodeAndCheckCodeSize: aCode [
	self writeCode: aCode.
	self checkCodeSize
]

{ #category : #'private-encoding' }
GIFReadWriter >> writeHeader [
	| byte |
	stream position = 0 ifTrue: 
		[ "For first image only"
		self nextPutAll: 'GIF89a' asByteArray.
		self writeWord: width.	"Screen Width"
		self writeWord: height.	"Screen Height"
		byte := 128.	"has color map"
		byte := byte bitOr: (bitsPerPixel - 1 bitShift: 5).	"color resolution"
		byte := byte bitOr: bitsPerPixel - 1.	"bits per pixel"
		self nextPut: byte.
		self nextPut: 0.	"background color."
		self nextPut: 0.	"reserved"
		colorPalette do: 
			[ :pixelValue | 
			self
				nextPut: ((pixelValue bitShift: -16) bitAnd: 255);
				nextPut: ((pixelValue bitShift: -8) bitAnd: 255);
				nextPut: (pixelValue bitAnd: 255) ].
		loopCount notNil ifTrue: 
			[ "Write a Netscape loop chunk"
			self nextPut: Extension.
			self nextPutAll: #(
					255
					11
					78
					69
					84
					83
					67
					65
					80
					69
					50
					46
					48
					3
					1
				) asByteArray.
			self writeWord: loopCount.
			self nextPut: 0 ] ].
	delay notNil | transparentIndex notNil ifTrue: 
		[ self
			nextPut: Extension;
			nextPutAll: #(249 4 ) asByteArray;
			nextPut: (transparentIndex 
					ifNil: [ 0 ]
					ifNotNil: [ 9 ]);
			writeWord: (delay ifNil: [ 0 ]);
			nextPut: (transparentIndex ifNil: [ 0 ]);
			nextPut: 0 ].
	self nextPut: ImageSeparator.
	self writeWord: 0.	"Image Left"
	self writeWord: 0.	"Image Top"
	self writeWord: width.	"Image Width"
	self writeWord: height.	"Image Height"
	byte := interlace 
		ifTrue: [ 64 ]
		ifFalse: [ 0 ].
	self nextPut: byte
]

{ #category : #'private-encoding' }
GIFReadWriter >> writeWord: aWord [
	self nextPut: (aWord bitAnd: 255).
	self nextPut: ((aWord bitShift: -8) bitAnd: 255).
	^aWord
]
