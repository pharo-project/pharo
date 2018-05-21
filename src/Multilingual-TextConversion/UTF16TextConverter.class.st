"
Text converter for UTF-16.  It supports the endianness and byte order mark.

The default is to not use a BOM, make sure to set this if using the converter to write to a location where encoding metadata is not present / specified by the protocol.

This usually means you want to write one when storing strings to files on disk, but not when transmitting encoded string to a web client, nor when sending data to a database.
"
Class {
	#name : #UTF16TextConverter,
	#superclass : #TextConverter,
	#instVars : [
		'useLittleEndian',
		'useByteOrderMark',
		'byteOrderMarkDone'
	],
	#category : #'Multilingual-TextConversion-TextConverter'
}

{ #category : #utilities }
UTF16TextConverter class >> encodingNames [

	^ #('utf-16' 'utf16' 'utf-16-le' 'utf-16-be') copy.

]

{ #category : #private }
UTF16TextConverter >> charFromStream: aStream withFirst: firstValue [

	^(firstValue < 16rD800 or: [firstValue > 16rDBFF])
		ifTrue:
			[Unicode value: firstValue]
		ifFalse:  "We have a surrogate pair"
			[| character1 character2 n secondValue |
			character1 := aStream basicNext ifNil: [^nil].
			character2 := aStream basicNext ifNil: [^nil].
			self useLittleEndian ifTrue: [ |tmp|
				tmp := character1.
				character1 := character2.
				character2 := tmp].
			secondValue := (character1 charCode << 8) + (character2 charCode).
			Unicode value: (firstValue - 16rD800) * 16r400 + (secondValue - 16rDC00) + 16r10000.]

]

{ #category : #private }
UTF16TextConverter >> checkBOMOn: aStream [
	"Write bom if applicable, and we are at beginning of stream"
	(self useByteOrderMark and: [byteOrderMarkDone not]) ifTrue: [
		aStream position = 0 ifTrue: [self next16BitValue: (16rFEFF) toStream: aStream].
		byteOrderMarkDone := true.
	].
]

{ #category : #initialization }
UTF16TextConverter >> initialize [
	super initialize.
	byteOrderMarkDone := false.
	useByteOrderMark := false.
	useLittleEndian := false.
]

{ #category : #private }
UTF16TextConverter >> next16BitValue: value toStream: aStream [

	| v1 v2 |
	v1 := (value >> 8) bitAnd: 16rFF.
	v2 := value bitAnd: 16rFF.

	self useLittleEndian ifTrue: [
		aStream basicNextPut: (Character value: v2).
		aStream basicNextPut: (Character value: v1).
	] ifFalse: [
		aStream basicNextPut: (Character value: v1).
		aStream basicNextPut: (Character value: v2).
	].

]

{ #category : #conversion }
UTF16TextConverter >> nextFromStream: aStream [

	| character1 character2 readBOM charValue |
	aStream isBinary ifTrue: [^ aStream basicNext].
	character1 := aStream basicNext ifNil: [^nil].
	character2 := aStream basicNext ifNil: [^nil].

	readBOM := false.
	(character1 asciiValue = 16rFF and: [character2 asciiValue = 16rFE]) ifTrue: 
		[self useLittleEndian: true.
		readBOM := true].
	(character1 asciiValue = 16rFE and: [character2 asciiValue = 16rFF]) ifTrue: 
		[self useLittleEndian: false.
		readBOM := true].

	readBOM ifTrue: 
		[self useByteOrderMark: true.
		character1 := aStream basicNext ifNil: [^nil].
		character2 := aStream basicNext ifNil: [^nil]].

	charValue := self useLittleEndian 
		ifTrue: [(character2 charCode bitShift: 8) + character1 charCode] 
		ifFalse: [(character1 charCode bitShift: 8) + character2 charCode].

	^ self charFromStream: aStream withFirst: charValue.

]

{ #category : #conversion }
UTF16TextConverter >> nextPut: aCharacter toStream: aStream [

	| v low high |
	
	self checkBOMOn: aStream.

	v := aCharacter charCode.
	v > 16rFFFF ifFalse: [
		self next16BitValue: v toStream: aStream.
		^ self.
	] ifTrue: [
		v := v - 16r10000.
		low := (v \\ 16r400) + 16rDC00.
		high := (v // 16r400) + 16rD800.
		self next16BitValue: high toStream: aStream.
		self next16BitValue: low toStream: aStream.
	]
]

{ #category : #conversion }
UTF16TextConverter >> nextPutAll: aString toStream: aStream [
	
	self checkBOMOn: aStream.
	^super nextPutAll: aString toStream: aStream
]

{ #category : #private }
UTF16TextConverter >> nextPutByteString: aString toStream: aStream [
	| utf16Bytes start |
	"No bytestring character will be a surrogate pair, so simply write the 2x size string with the correct endianness"
	utf16Bytes := String new: aString size* 2.
	start := useLittleEndian ifTrue: [1] ifFalse: [2].
	start to: utf16Bytes size by: 2 do: [:ix | utf16Bytes at: ix put: (aString at: ix +1 // 2 )].
	aStream basicNextPutAll: utf16Bytes.
	
]

{ #category : #private }
UTF16TextConverter >> swapLatin1EncodingByteOrder [
	latin1Encodings := latin1Encodings collect: [:each | each reverse]
]

{ #category : #accessing }
UTF16TextConverter >> useByteOrderMark [

	^ useByteOrderMark

]

{ #category : #accessing }
UTF16TextConverter >> useByteOrderMark: aBoolean [

	useByteOrderMark := aBoolean.

]

{ #category : #accessing }
UTF16TextConverter >> useLittleEndian [

	^ useLittleEndian

]

{ #category : #accessing }
UTF16TextConverter >> useLittleEndian: aBoolean [
	aBoolean = self useLittleEndian ifFalse: [self swapLatin1EncodingByteOrder].
	useLittleEndian := aBoolean.

]
