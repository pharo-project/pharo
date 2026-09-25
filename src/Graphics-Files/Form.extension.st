Extension { #name : 'Form' }

{ #category : '*Graphics-Files' }
Form class >> fromBase64String: aBase64EncodedString [
	"Convenient method to instantiate a Form from an image content encoded in base64 (used to store image data in a method for example".
	"Encoded string can be obtained with:
	(Base64MimeConverter mimeEncode: 'picture.png' asFileReference binaryReadStream) contents."

	^ self fromBinaryStream: aBase64EncodedString base64Decoded asByteArray readStream
]

{ #category : '*Graphics-Files' }
Form class >> fromBinaryStream: aBinaryStream [

	"Read a Form or ColorForm from given file, using the first byte of the file to guess its format. Currently handles: GIF, uncompressed BMP, and new DisplayObject writeOn: formats, JPEG, and PCX. Return nil if the file could not be read or was of an unrecognized format."

	| firstByte |
	firstByte := aBinaryStream peek.
	firstByte = 1 ifTrue: [ ^ self error: 'old ST80 format not supported' ].
	firstByte = 2 ifTrue: [ ^ self new readFrom: aBinaryStream ].

	"Try for JPG, GIF, or PCX..."
	"Note: The following call closes the stream."
	^ ImageReadWriter formFromStream: aBinaryStream
]

{ #category : '*Graphics-Files' }
Form class >> fromFileNamed: fileName [
	"Read a Form or ColorForm from the given file."

	^ fileName asFileReference
			binaryReadStreamDo: [ :in | self fromBinaryStream: in ]
]

{ #category : '*Graphics-Files' }
Form >> readNativeResourceFrom: byteStream [
	| img |
	img := [ ImageReadWriter formFromStream: byteStream ]
		on: Error
		do: [ :ex | ^ nil ].
	(img isColorForm and: [ self isColorForm ])
		ifTrue: [ | cc |
			cc := img colors.
			img colors: nil.
			img displayOn: self.
			img colors: cc ]
		ifFalse: [ img displayOn: self ]
]

{ #category : '*Graphics-Files' }
Form >> readResourceFrom: aStream [
	"Store a resource representation of the receiver on aStream.
	Must be specific to the receiver so that no code is filed out."

	| bitsSize msb |
	(aStream next: 4) asString = self resourceTag ifFalse: [
			aStream position: aStream position - 4.
			^ self readNativeResourceFrom: aStream ].
	self width: (aStream nextNumber: 4).
	self height: (aStream nextNumber: 4).
	self basicDepth: (aStream nextNumber: 4).
	bitsSize := aStream nextNumber: 4.
	bitsSize = 0 ifFalse: [
			self bits: (aStream next: bitsSize).
			^ self ].
	msb := (aStream nextNumber: 4) = 1.
	bitsSize := aStream nextNumber: 4.
	self bits: (Bitmap new: self bitsSize).
	(Form extent: self width @ self height depth: self basicDepth bits: (aStream next: bitsSize * 4)) displayOn: self.
	msb = EndianDetector isBigEndian ifFalse: [ Bitmap swapBytesIn: self bits from: 1 to: self bits size ]
]

{ #category : '*Graphics-Files' }
Form >> resourceTag [
	^'FORM'
]

{ #category : '*Graphics-Files' }
Form >> storeResourceOn: aStream [
	"Store a resource representation of the receiver on aStream.
	Must be specific to the receiver so that no code is filed out."

	self hibernate.
	aStream nextPutAll: self resourceTag asByteArray. "tag"
	aStream nextNumber: 4 put: self width.
	aStream nextNumber: 4 put: self height.
	aStream nextNumber: 4 put: self basicDepth.
	(self bits isMemberOf: ByteArray) ifFalse: [ "must store bitmap"
			aStream nextNumber: 4 put: 0. "tag"
			aStream nextNumber: 4 put: (EndianDetector isBigEndian
					 ifTrue: [ 1 ]
					 ifFalse: [ 0 ]) ].
	aStream nextNumber: 4 put: self bits size.
	aStream nextPutAll: self bits
]

{ #category : '*Graphics-Files' }
Form >> writeBMPfileNamed: fName [  "Display writeBMPfileNamed: 'display.bmp'"
	BMPReadWriter putForm: self onFileNamed: fName
]

{ #category : '*Graphics-Files' }
Form >> writeJPEGfileNamed: fileName [
	"Write a JPEG file to the given filename using default settings"

	self writeJPEGfileNamed: fileName progressive: false

"
Display writeJPEGfileNamed: 'display.jpeg'
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg'
"
]

{ #category : '*Graphics-Files' }
Form >> writeJPEGfileNamed: fileName  progressive: aBoolean [
	"Write a JPEG file to the given filename using default settings.  Make it progressive or not, depending on the boolean argument"

	JPEGReadWriter putForm: self quality: -1 "default" progressiveJPEG: aBoolean onFileNamed: fileName

"
Display writeJPEGfileNamed: 'display.jpeg' progressive: false.
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg' progressive: true
"
]

{ #category : '*Graphics-Files' }
Form >> writeOnMovie: file [
	"Write just my bits on the file."

	self unhibernate.
	self bits writeUncompressedOn: file
]

{ #category : '*Graphics-Files' }
Form >> writePNGFileNamed: fileName [
	PNGReadWriter putForm: self onFileNamed: fileName
]
