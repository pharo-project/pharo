"
I am ZnNullEncoder, a concrete subclass of ZnCharacterEncoder.
I perform no encoding or decoding at all for all characters with a code value below 256.

Note that in principle I could handle Latin1 (ISO-8859-1) or ASCII, although that is not completely correct. To get maximum efficiency, it remains an option.
	
Part of Zinc HTTP Components.
"
Class {
	#name : #ZnNullEncoder,
	#superclass : #ZnCharacterEncoder,
	#category : #Zinc-Character-Encoding-Core
}

{ #category : #accessing }
ZnNullEncoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string.
	Note that in principle I could handle latin1 (iso-8859-1) and ASCII, 
	although that is not completely correct. 
	To get maximum efficiency, it remains an option."
	
	^ (self canonicalEncodingIdentifier: string) = 'null'
]

{ #category : #accessing }
ZnNullEncoder class >> knownEncodingIdentifiers [
	^ #( null )
]

{ #category : #'instance creation' }
ZnNullEncoder class >> newForEncoding: string [
	"No further parametrization needed"
	
	^ self new
]

{ #category : #converting }
ZnNullEncoder >> backOnStream: stream [
	"Move back one character on stream"

	stream back
]

{ #category : #convenience }
ZnNullEncoder >> decodeBytes: bytes [
	"Decode bytes and return the resulting string"
	"Overwritten for performance reasons"

	^ bytes asString
]

{ #category : #convenience }
ZnNullEncoder >> encodeString: string [
	"Encode string and return the resulting byte array"
	"Overwritten for performance reasons"

	^ string asByteArray
]

{ #category : #converting }
ZnNullEncoder >> encodedByteCountFor: character [
	"Return how many bytes are needed to encode character"
	"Overwritten for performance reasons"

	^ 1
]

{ #category : #converting }
ZnNullEncoder >> encodedByteCountForCodePoint: codePoint [
	"Return how many bytes are needed to encode integer code point"

	^ 1
]

{ #category : #convenience }
ZnNullEncoder >> encodedByteCountForCodePoints: codePoints [
	"Return the exact number of bytes it would take to encode codePoints as a byte array"
	"Overwritten for performance reasons"

	^ codePoints size
]

{ #category : #convenience }
ZnNullEncoder >> encodedByteCountForString: string [
	"Return the exact number of bytes it would take to encode string as a byte array"
	"Overwritten for performance reasons"

	^ string size
]

{ #category : #accessing }
ZnNullEncoder >> identifier [
	^ #ascii
]

{ #category : #convenience }
ZnNullEncoder >> next: count putAll: string startingAt: offset toStream: stream [
	"Write count bytes from string starting at offset to stream."
	"Overwritten for performance reasons"

	string isByteString
		ifTrue: [ 
			offset to: offset + count - 1 do: [ :index | 
				stream nextPut: (string byteAt: index) ] ]
		ifFalse: [ 
			super next: count putAll: string startingAt: offset toStream: stream ]
]

{ #category : #converting }
ZnNullEncoder >> nextCodePointFromStream: stream [
	"Read and return the next integer code point from stream"

	^ stream next
]

{ #category : #converting }
ZnNullEncoder >> nextPutCodePoint: codePoint toStream: stream [
	"Write the encoding for Integer code point to stream"

	codePoint < 256
		ifTrue: [ stream nextPut: codePoint ]
		ifFalse: [ self errorOutsideRange ]
]

{ #category : #convenience }
ZnNullEncoder >> readInto: string startingAt: offset count: requestedCount fromStream: stream [
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"
	"Overwritten for performance reasons"

	offset to: offset + requestedCount - 1 do: [ :index |
		stream atEnd ifTrue: [ ^ index - offset ].  
		string byteAt: index put: stream next ].
	^ requestedCount
]
