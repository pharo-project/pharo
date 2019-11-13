"
I am ImageReadWriter. 
I read an encoded image from a binary stream and return its as a Form. 
I write and encode an image as Form to a binary stream.
I am an abstract class, my subclasses implement concrete formats.
I can autodetect the format of some encoded images, so you can use me directly.

Usage

	ImageReadWriter formFromFileNamed: 'test.png'
	ImageReadWriter formFromStream: 'test.png' asFileReference binaryReadStream

Implementation 

Instance Variables:
	stream		<ReadStream|WriteStream>	binary stream that I read from or write to

Subclasses must implement the following messages:
	accessing
		nextImage
		nextPutImage:
	testing
		canUnderstand         (or my class side's #understandsImageFormat:)
			
(original) copyright (c) Kazuki Yasumatsu, 1995. All rights reserved.

"
Class {
	#name : #ImageReadWriter,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : #'Graphics-Files'
}

{ #category : #'image reading/writing' }
ImageReadWriter class >> allTypicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that my subclasses can read might commonly have"

	"ImageReadWriter allTypicalFileExtensions"

	| extensions |
	extensions := Set new.
	self allSubclassesDo: [ :cls | extensions addAll: cls typicalFileExtensions ].
	^ extensions
]

{ #category : #'image reading/writing' }
ImageReadWriter class >> formFromFileNamed: fileName [ 
	"Answer a Form of the image stored (encoded) on the file with fileName.
	This will (normally) autodetect the format and delegate to the correct concrete subclass."
	
	"ImageReadWriter formFromFileNamed: 'test.png'"
	"ImageReadWriter formFromFileNamed: 'test.jpg'"
	
	^ fileName asFileReference 
		binaryReadStreamDo: [ :in | self formFromStream: in ]
]

{ #category : #'image reading/writing' }
ImageReadWriter class >> formFromStream: aBinaryReadStream [
	"Answer a Form of the image stored on aBinaryReadStream.
	This will (normally) autodetect the format and delegate to the correct concrete subclass.
	Closes aBinaryReadStream when done."
	
	"ImageReadWriter formFromStream: 'test.png' asFileReference binaryReadStream"
	"ImageReadWriter formFromStream: 'test.jpg' asFileReference binaryReadStream"

	| positionableReadStream reader readerClass form |
	positionableReadStream := ZnPositionableReadStream on: aBinaryReadStream.
	readerClass := self readerClassFromStream: positionableReadStream.
	reader := readerClass new on: positionableReadStream.
	form := reader nextImage.
	aBinaryReadStream close.
	^ form
]

{ #category : #'instance creation' }
ImageReadWriter class >> on: binaryStream [
	"Answer an instance of the receiver for encoding and/or decoding an image on binaryStream.
	For reading, you should pass along a read stream, for writing a write stream."

	^ self new on: binaryStream
]

{ #category : #'image reading/writing' }
ImageReadWriter class >> putForm: aForm onFileNamed: fileName [ 
	"Store (encode) the image of the Form aForm on a file of with fileName.
	Use a concrete subclass to select the desired format."
	
	"PNGReadWriter putForm: PolymorphSystemSettings pharoLogoForm onFileNamed: 'test.png'"
	"PluginBasedJPEGReadWriter putForm: PolymorphSystemSettings pharoLogoForm onFileNamed: 'test.jpg'"
	
	fileName asFileReference 
		binaryWriteStreamDo: [ :out | (self on: out) nextPutImage: aForm ]
]

{ #category : #'image reading/writing' }
ImageReadWriter class >> putForm: aForm onStream: aBinaryWriteStream [ 
	"Store (encode) the image of the Form aForm on aBinaryWriteStream.
	Use a concrete subclass to select the desired format.
	Closes aBinaryWriteStream when done."

	"PNGReadWriter putForm: PolymorphSystemSettings pharoLogoForm onStream: 'test.png' asFileReference binaryWriteStream"
	"PluginBasedJPEGReadWriter putForm: PolymorphSystemSettings pharoLogoForm onStream: 'test.jpg' asFileReference binaryWriteStream"

	| writer |
	writer := self on: aBinaryWriteStream.
	writer nextPutImage: aForm.
	writer close
]

{ #category : #'image reading/writing' }
ImageReadWriter class >> readerClassFromStream: positionableReadStream [
	| readerClass |
	readerClass := self withAllSubclasses
		detect: [ :subclass | 
			positionableReadStream savingPositionDo: [ 
				subclass understandsImageFormat: positionableReadStream ] ]
		ifNone: [ positionableReadStream close.
			^ self error: 'image format not recognized' ].
	^ readerClass
]

{ #category : #'image reading/writing' }
ImageReadWriter class >> typicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that I can read might commonly have"

	^ #()
]

{ #category : #'image reading/writing' }
ImageReadWriter class >> understandsImageFormat: aStream [
	^ [ (self new on: aStream) understandsImageFormat ]
		on: Error
		do: [ :ex | ex return: false ]
]

{ #category : #'image reading/writing' }
ImageReadWriter >> allTypicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that my subclasses can read might commonly have"
	"ImageReadWriter allTypicalFileExtensions"
	| extensions |
	extensions := Set new.
	self allSubclassesDo: [ :cls | extensions addAll: cls typicalFileExtensions ].
	^ extensions
]

{ #category : #'stream access' }
ImageReadWriter >> atEnd [

	^stream atEnd
]

{ #category : #private }
ImageReadWriter >> changePadOfBits: bits width: width height: height depth: depth from: oldPad to: newPad [ 
	"Change padding size of bits."
	| srcRowByteSize dstRowByteSize newBits srcRowBase rowEndOffset |
	(#(8 16 32 ) includes: oldPad) ifFalse: [ ^ self error: 'Invalid pad: ' , oldPad printString ].
	(#(8 16 32 ) includes: newPad) ifFalse: [ ^ self error: 'Invalid pad: ' , newPad printString ].
	srcRowByteSize := (width * depth + oldPad - 1) // oldPad * (oldPad / 8).
	srcRowByteSize * height = bits size ifFalse: [ ^ self error: 'Incorrect bitmap array size.' ].
	dstRowByteSize := (width * depth + newPad - 1) // newPad * (newPad / 8).
	newBits := ByteArray new: dstRowByteSize * height.
	srcRowBase := 1.
	rowEndOffset := dstRowByteSize - 1.
	1 
		to: newBits size
		by: dstRowByteSize
		do: 
			[ :dstRowBase | 
			newBits 
				replaceFrom: dstRowBase
				to: dstRowBase + rowEndOffset
				with: bits
				startingAt: srcRowBase.
			srcRowBase := srcRowBase + srcRowByteSize ].
	^ newBits
]

{ #category : #'stream access' }
ImageReadWriter >> close [
	
	stream close
]

{ #category : #'stream access' }
ImageReadWriter >> contents [

	^stream contents
]

{ #category : #'stream access' }
ImageReadWriter >> cr [

	^stream nextPut: Character cr asInteger
]

{ #category : #private }
ImageReadWriter >> hasMagicNumber: aByteArray [ 
	| position |
	position := stream position.
	(stream next: aByteArray size) = aByteArray
		ifTrue: [ ^ true ].
	stream position: position.
	^ false
]

{ #category : #testing }
ImageReadWriter >> isAnimated [
	^ false
]

{ #category : #'stream access' }
ImageReadWriter >> lf [
	"PPM and PBM are used LF as CR."

	^stream nextPut: Character lf asInteger
]

{ #category : #'stream access' }
ImageReadWriter >> next [

	^stream next
]

{ #category : #'stream access' }
ImageReadWriter >> next: size [

	^stream next: size
]

{ #category : #accessing }
ImageReadWriter >> nextImage [
	"Decode an encoded image from my stream and return it as a Form"

	^ self subclassResponsibility
]

{ #category : #'stream access' }
ImageReadWriter >> nextIntegerOfSize: numberOfBytes signed: signed bigEndian: bigEndian [
	"Assuming the receiver is a stream of bytes, read the next integer of size numberOfBytes.
	If bigEndian is true, use network byte order, most significant byte first, 
	else use little endian order, least significant byte first.
	If signed is true, interpret as a two-complement signed value, 
	else interpret as a plain unsigned value."
	
	| value |
	value := 0.
	bigEndian
		ifTrue: [ 
			(numberOfBytes - 1) * 8 to: 0 by: -8 do: [ :shift |
				value := value + (self next bitShift: shift) ] ]
		ifFalse: [ 
			0 to: (numberOfBytes - 1) * 8 by: 8 do: [ :shift |
				value := value + (self next bitShift: shift) ] ].
	^ (signed and: [ (value bitAt: numberOfBytes * 8) = 1 ])
		ifTrue: [ value - (1 << (numberOfBytes * 8)) ]
		ifFalse: [ value ]
]

{ #category : #'stream access' }
ImageReadWriter >> nextIntegerOfSize: numberOfBytes signed: signed bigEndian: bigEndian put: value [
	"Assuming the receiver is a stream of bytes, write value as the next integer of size numberOfBytes.
	If bigEndian is true, use network byte order, most significant byte first, 
	else use little endian order, least significant byte first.
	If signed is true, encode as a two-complement signed value, 
	else encode as a plain unsigned value."
	
	| unsignedValue |
	unsignedValue := (signed and: [ value negative ])
		ifTrue: [ (1 << (numberOfBytes * 8)) + value ] 
		ifFalse: [ value ].
	(unsignedValue between: 0 and: (2 ** (numberOfBytes * 8)) - 1)
		ifFalse: [ DomainError signalFrom: 0 to: (2 ** (numberOfBytes * 8)) - 1 ].
	bigEndian
		ifTrue: [ 
			numberOfBytes to: 1 by: -1 do: [ :index |
				self nextPut: (unsignedValue byteAt: index) ] ]
		ifFalse: [ 
			1 to: numberOfBytes do: [ :index |
				self nextPut: (unsignedValue byteAt: index) ] ].
	^ value
]

{ #category : #'stream access' }
ImageReadWriter >> nextLittleEndianNumber: numberOfBytes [
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: false
]

{ #category : #'stream access' }
ImageReadWriter >> nextLittleEndianNumber: numberOfBytes put: integer [
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: false put: integer
]

{ #category : #'stream access' }
ImageReadWriter >> nextLong [
	"Read a 32-bit quantity from the input stream."

	^(stream next bitShift: 24) + (stream next bitShift: 16) +
		(stream next bitShift: 8) + stream next
]

{ #category : #'stream access' }
ImageReadWriter >> nextLongPut: a32BitW [
	"Write out a 32-bit integer as 32 bits."

	stream nextPut: ((a32BitW bitShift: -24) bitAnd: 16rFF).
	stream nextPut: ((a32BitW bitShift: -16) bitAnd: 16rFF).
	stream nextPut: ((a32BitW bitShift: -8) bitAnd: 16rFF).
	stream nextPut: (a32BitW bitAnd: 16rFF).
	^a32BitW
]

{ #category : #'stream access' }
ImageReadWriter >> nextPut: aByte [

	^stream nextPut: aByte
]

{ #category : #'stream access' }
ImageReadWriter >> nextPutAll: aByteArray [

	^stream nextPutAll: aByteArray
]

{ #category : #accessing }
ImageReadWriter >> nextPutImage: imageForm [
	"Encoding imageForm onto my stream"

	self subclassResponsibility
]

{ #category : #'stream access' }
ImageReadWriter >> nextWord [
	"Read a 16-bit quantity from the input stream."

	^(stream next bitShift: 8) + stream next
]

{ #category : #'stream access' }
ImageReadWriter >> nextWordPut: a16BitW [
	"Write out a 16-bit integer as 16 bits."

	stream nextPut: ((a16BitW bitShift: -8) bitAnd: 16rFF).
	stream nextPut: (a16BitW bitAnd: 16rFF).
	^a16BitW
]

{ #category : #private }
ImageReadWriter >> on: binaryStream [ 
	stream := binaryStream
]

{ #category : #'stream access' }
ImageReadWriter >> peekFor: aValue [

	^stream peekFor: aValue
]

{ #category : #'stream access' }
ImageReadWriter >> position [

	^stream position
]

{ #category : #'stream access' }
ImageReadWriter >> position: anInteger [

	^stream position: anInteger
]

{ #category : #'stream access' }
ImageReadWriter >> size [

	^stream size
]

{ #category : #'stream access' }
ImageReadWriter >> skip: anInteger [

	^stream skip: anInteger
]

{ #category : #'stream access' }
ImageReadWriter >> space [

	^stream nextPut: Character space asInteger
]

{ #category : #'stream access' }
ImageReadWriter >> tab [

	^stream nextPut: Character tab asInteger
]

{ #category : #testing }
ImageReadWriter >> understandsImageFormat [
	"Test to see if the image stream format is understood by this decoder.
	This should be implemented in each subclass of ImageReadWriter so that
	a proper decoder can be selected without ImageReadWriter having to know
	about all possible image file types."

	^ false
]

{ #category : #private }
ImageReadWriter >> unpackBits: bits depthTo8From: depth with: width height: height pad: pad [ 
	"Unpack bits of depth 1, 2, or 4 image to it of depth 8 image."
	| bitMask pixelInByte bitsWidth upBitsWidth stopWidth trailingSize upBits bitIndex upBitIndex val |
	(#(1 2 4 ) includes: depth) ifFalse: [ ^ self error: 'depth must be 1, 2, or 4' ].
	(#(8 16 32 ) includes: pad) ifFalse: [ ^ self error: 'pad must be 8, 16, or 32' ].
	bitMask := (1 bitShift: depth) - 1.
	pixelInByte := 8 / depth.
	bitsWidth := (width * depth + pad - 1) // pad * (pad / 8).
	upBitsWidth := (width * 8 + pad - 1) // pad * (pad / 8).
	stopWidth := (width * depth + 7) // 8.
	trailingSize := width - ((stopWidth - 1) * pixelInByte).
	upBits := ByteArray new: upBitsWidth * height.
	1 
		to: height
		do: 
			[ :i | 
			bitIndex := (i - 1) * bitsWidth.
			upBitIndex := (i - 1) * upBitsWidth.
			1 
				to: stopWidth - 1
				do: 
					[ :j | 
					val := bits at: (bitIndex := bitIndex + 1).
					upBitIndex := upBitIndex + pixelInByte.
					1 
						to: pixelInByte
						do: 
							[ :k | 
							upBits 
								at: upBitIndex - k + 1
								put: (val bitAnd: bitMask).
							val := val bitShift: depth negated ] ].
			val := (bits at: (bitIndex := bitIndex + 1)) bitShift: depth negated * (pixelInByte - trailingSize).
			upBitIndex := upBitIndex + trailingSize.
			1 
				to: trailingSize
				do: 
					[ :k | 
					upBits 
						at: upBitIndex - k + 1
						put: (val bitAnd: bitMask).
					val := val bitShift: depth negated ] ].
	^ upBits
]
