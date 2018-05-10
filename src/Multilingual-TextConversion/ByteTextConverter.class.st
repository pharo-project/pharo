"
A ByteTextConverter is the abstract class for text converters on single byte encodings.
"
Class {
	#name : #ByteTextConverter,
	#superclass : #TextConverter,
	#classInstVars : [
		'byteToUnicode',
		'unicodeToByte'
	],
	#category : #'Multilingual-TextConversion'
}

{ #category : #'class initialization' }
ByteTextConverter class >> byteToUnicodeSpec [
	"Specify a table mapping the entries 0x80 to 0xFF to their unicode counterparts by returning a 128 element array.
	The entries 0x00 to 0x7F map to identical values so we don't need to specify them.
	See #generateByteToUnicodeSpec: for an automated way to generate these mappings."

	self subclassResponsibility
]

{ #category : #accessing }
ByteTextConverter class >> byteToUnicodeTable [
	"Return the table mapping from my byte based encoding to unicode"
	
	^ byteToUnicode
]

{ #category : #'class initialization' }
ByteTextConverter class >> initialize [
	"ByteTextConverter initialize"

	self allSubclassesDo: [:subclass |
		subclass initializeTables]
]

{ #category : #'class initialization' }
ByteTextConverter class >> initializeTables [
	"Initialize the mappings to and from Unicode."

	byteToUnicode := Array new: 128.
	unicodeToByte := Dictionary new.
	"Mind the offset because first 128 characters are not stored into byteToUnicodeSpec"
	"Some entries are nil, make them pass-through for compatibility, for now"
	self byteToUnicodeSpec keysAndValuesDo: [ :index :unicode | | unicodeValue |
		unicodeValue := unicode ifNil: [ 127 + index ] ifNotNil: [ unicode ].
		byteToUnicode at: index put: (Character value: unicodeValue).
		unicodeToByte at: unicodeValue put: (127 + index) asCharacter ]
]

{ #category : #accessing }
ByteTextConverter class >> languageEnvironment [
	self subclassResponsibility
]

{ #category : #accessing }
ByteTextConverter class >> unicodeToByteTable [
	"Return the table mapping from unicode to my byte based encoding"
	
	^ unicodeToByte
]

{ #category : #conversion }
ByteTextConverter >> byteToUnicode: char [
	"Map from my byte based encoding to unicode.
	Due to the leading char encoding this is not strictly true, but hopefully at some point we can get rid of the leading char overhead."
	| value |
	value := char charCode.

	value < 128
		ifTrue: [^ char].
	value > 255
		ifTrue: [^ char].
	^self class byteToUnicodeTable at: (value - 128 + 1)
]

{ #category : #conversion }
ByteTextConverter >> nextFromStream: aStream [
	"Read the next byte (we are only dealing with byte based encodings here) character from aStream and return the result converted to unicode."

	| byteOrChar |
	byteOrChar := aStream basicNext.
	aStream isBinary
		ifTrue: [^byteOrChar].
	^byteOrChar
		ifNotNil: [self byteToUnicode: byteOrChar]
]

{ #category : #conversion }
ByteTextConverter >> nextPut: unicodeCharacter toStream: aStream [
	"Write the unicode character to aStream."

	aStream isBinary
		ifTrue: [aStream basicNextPut: unicodeCharacter charCode]
		ifFalse: [aStream basicNextPut: (self unicodeToByte: unicodeCharacter)]
]

{ #category : #conversion }
ByteTextConverter >> unicodeToByte: unicodeChar [

	^unicodeChar charCode < 128
		ifTrue: [unicodeChar]
		ifFalse: [self class unicodeToByteTable at: unicodeChar charCode ifAbsent: [0 asCharacter]]
]
