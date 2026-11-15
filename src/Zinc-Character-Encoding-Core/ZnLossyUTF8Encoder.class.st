Class {
	#name : 'ZnLossyUTF8Encoder',
	#superclass : 'ZnUTF8Encoder',
	#category : 'Zinc-Character-Encoding-Core',
	#package : 'Zinc-Character-Encoding-Core'
}

{ #category : 'accessing' }
ZnLossyUTF8Encoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"

	^ (self canonicalEncodingIdentifier: string) = 'utf8lossy'
]

{ #category : 'accessing' }
ZnLossyUTF8Encoder class >> knownEncodingIdentifiers [
	^ #( 'utf8lossy' )
]

{ #category : 'convenience' }
ZnLossyUTF8Encoder >> decodeBytesSingleReplacement: bytes [
	"Decode bytes and return the resulting string.
	This variant of #decodeBytes: will only ever use 
	a single replacement character for each illegal UTF-8 sequence"

	| byteStream replaced replacement char |
	byteStream := bytes readStream.
	replaced := false.
	replacement := self replacementCodePoint asCharacter.
	^ String streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [
			char := self nextFromStream: byteStream.
			char = replacement
				ifTrue: [
					replaced 
						ifFalse: [
							replaced := true.
							stream nextPut: replacement ] ]
				ifFalse: [
					replaced := false.
					stream nextPut: char ] ] ]
]

{ #category : 'error handling' }
ZnLossyUTF8Encoder >> errorIllegalContinuationByte: byte from: stream [
	^ self replacementCodePoint
]

{ #category : 'error handling' }
ZnLossyUTF8Encoder >> errorIllegalLeadingByte: byte from: stream [
	^ self replacementCodePoint
]

{ #category : 'error handling' }
ZnLossyUTF8Encoder >> errorIncompleteFrom: stream [
	^ self replacementCodePoint
]

{ #category : 'error handling' }
ZnLossyUTF8Encoder >> errorOutsideRange: codePoint from: stream [
	^ self replacementCodePoint
]

{ #category : 'error handling' }
ZnLossyUTF8Encoder >> errorOutsideRange: codePoint to: stream [
	stream nextPutAll: self replacementCodePointEncoded
]

{ #category : 'error handling' }
ZnLossyUTF8Encoder >> errorOutsideRangeByteCount: codePoint [
	^ 0
]

{ #category : 'error handling' }
ZnLossyUTF8Encoder >> errorOverlong: codePoint from: stream [
	^ self replacementCodePoint
]

{ #category : 'accessing' }
ZnLossyUTF8Encoder >> identifier [
	^ 'utf8lossy'
]

{ #category : 'accessing' }
ZnLossyUTF8Encoder >> replacementCodePoint [
	"Return the code point for the Unicode Replacement Character U+FFFD"
	
	^ 16rFFFD
]

{ #category : 'accessing' }
ZnLossyUTF8Encoder >> replacementCodePointEncoded [
	"Return the encoded utf-8 byte sequence for the Unicode Replacement Character U+FFFD"
	
	"self replacementCodePoint asCharacter utf8Encoded"
	
	^ #[239 191 189]
]
