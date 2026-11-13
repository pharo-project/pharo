"
A ZLib read stream
"
Class {
	#name : 'ZLibReadStream',
	#superclass : 'FastInflateStream',
	#instVars : [
		'dictionary'
	],
	#category : 'Compression-Streams',
	#package : 'Compression',
	#tag : 'Streams'
}

{ #category : 'instance creation' }
ZLibReadStream class >> on: aCollectionOrStream dictionary: aByteArray [
	"Answer a stream decompressing ZLIB data compressed with the preset dictionary aByteArray (RFC 1950, FDICT)"

	^ self basicNew
		  setDictionary: aByteArray;
		  on: aCollectionOrStream
]

{ #category : 'initialization' }
ZLibReadStream >> on: aCollection from: firstIndex to: lastIndex [
	"Check the header of the ZLib stream."

	| method byte |
	super on: aCollection from: firstIndex to: lastIndex.
	crc := 1.
	method := self nextBits: 8.
	(method bitAnd: 15) = 8 ifFalse: [ ^ self error: 'Unknown compression method' ].
	(method bitShift: -4) + 8 > 15 ifTrue: [ ^ self error: 'Invalid window size' ].
	byte := self nextBits: 8.
	(method bitShift: 8) + byte \\ 31 = 0 ifFalse: [ ^ self error: 'Incorrect header' ].
	(byte anyMask: 32) ifTrue: [ self readPresetDictionary ]
]

{ #category : 'initialization' }
ZLibReadStream >> readPresetDictionary [
	"FDICT: the header is followed by the Adler-32 of the preset dictionary, which fills the window before decompressing (RFC 1950, section 2.2)"

	| dictionaryId |
	dictionaryId := (1 to: 4) inject: 0 into: [ :value :each | (value bitShift: 8) + (self nextBits: 8) ].
	dictionary ifNil: [ ^ self error: 'Need preset dictionary (Adler-32 ' , dictionaryId printStringHex , ')' ].
	dictionary size > 32768 ifTrue: [ ^ self error: 'Preset dictionary larger than the window' ].
	(Adler32
		 update: 1
		 from: 1
		 to: dictionary size
		 in: dictionary asByteArray) = dictionaryId ifFalse: [
		^ self error: 'Wrong preset dictionary (expected Adler-32 ' , dictionaryId printStringHex , ')' ].
	collection
		replaceFrom: 1
		to: dictionary size
		with: (collection isString
				 ifTrue: [ dictionary asString ]
				 ifFalse: [ dictionary asByteArray ])
		startingAt: 1.
	position := readLimit := dictionary size
]

{ #category : 'initialization' }
ZLibReadStream >> setDictionary: aByteArray [

	dictionary := aByteArray
]

{ #category : 'crc' }
ZLibReadStream >> updateCrc: oldCrc from: start to: stop in: aCollection [
	"Answer an updated CRC for the range of bytes in aCollection"
	^ Adler32 update: oldCrc from: start to: stop in: aCollection
]

{ #category : 'crc' }
ZLibReadStream >> verifyCrc [
	| stored |
	stored := 0.
	24 to: 0 by: -8 do: [ :i |
		sourcePos >= sourceLimit ifTrue: [ ^ self crcError: 'No checksum (proceed to ignore)' ].
		stored := stored + (self nextByte bitShift: i) ].
	stored = crc ifFalse: [ ^ self crcError: 'Wrong checksum (proceed to ignore)' ].
	^stored
]
