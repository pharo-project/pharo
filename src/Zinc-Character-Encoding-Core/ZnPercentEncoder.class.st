"
I am ZnPercentEncoder.
I implement RFC 3986 percent encoding and decoding.

All characters that are not part of a safe set are encoded using a percent (%) followed by a two digit hexadecimal number of a byte value. Non-ASCII characters are first encoded, normally using UTF-8.

My #encode: and #decode: messages work from String to String.

My decoder will accept + as an encoding for a space by default.

See also http://en.wikipedia.org/wiki/Percent-encoding

Part of Zinc HTTP Components

"
Class {
	#name : #ZnPercentEncoder,
	#superclass : #Object,
	#instVars : [
		'characterEncoder',
		'safeSet',
		'decodePlusAsSpace'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnPercentEncoder class >> rfc3986UnreservedCharacters [
	"Return the unreserved characters according to RFC 3986 section 2.3.
	This is the most narrow safe set to be used in a better safe than sorry approach."

	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~'
]

{ #category : #accessing }
ZnPercentEncoder >> characterEncoder [
	"Return the character encoder that I currently use.
	If not set, I will default to using UTF-8."
	
	^ characterEncoder ifNil: [ characterEncoder := ZnDefaultCharacterEncoder value ]
]

{ #category : #'initialize-release' }
ZnPercentEncoder >> characterEncoder: object [
	"Set the character encoding to use to object."
	
	characterEncoder := object
]

{ #category : #converting }
ZnPercentEncoder >> decode: string [
	"Decode a percent encoded string and return the resulting byte array.
	After percent decoding I will use my character encoder to convert byte values to characters."

	| bytes stringStream |
	stringStream := string readStream.
	bytes := ByteArray streamContents: [ :byteStream | 
		self decode: stringStream to: byteStream ].
	^ self characterEncoder decodeBytes: bytes
]

{ #category : #converting }
ZnPercentEncoder >> decode: stringStream to: byteStream [
	| char |
	self decodePlusAsSpace.
	[ stringStream atEnd ]
		whileFalse: [ 
			((char := stringStream next) == $+ and: [ decodePlusAsSpace ])
				ifTrue: [ byteStream nextPut: 32 ]
				ifFalse: [ 
					char == $%
						ifTrue: [ byteStream nextPut: (self readHexFrom: stringStream) ]
						ifFalse: [ 
							char charCode < 128
								ifTrue: [ byteStream nextPut: char charCode ]
								ifFalse: [ self errorAsciiCharacterExpected ] ] ] ]
]

{ #category : #accessing }
ZnPercentEncoder >> decodePlusAsSpace [
	"Return if $+ on input should be decoded as Character space.
	This is normally only done application/x-www-form-urlencoded data,
	but is is on by default anyway."
	
	^ decodePlusAsSpace ifNil: [ decodePlusAsSpace := true ]
]

{ #category : #'initialize-release' }
ZnPercentEncoder >> decodePlusAsSpace: boolean [
	"When boolean is true, $+ on input will be decoded as Character space.
	Else $+ is treated as a normal character, filtered by the safe set.
	This is normally only done application/x-www-form-urlencoded data,
	but is is on by default anyway."
	
	decodePlusAsSpace := boolean
]

{ #category : #converting }
ZnPercentEncoder >> encode: string [
	"Encode string using percent encoding and return the resulting string.
	I will use my character encoder to convert string to bytes and then
	percent encode all byte values that are not in my safe set."

	^ String streamContents: [ :stream | 
		self encode: string readStream to: stream ]
]

{ #category : #converting }
ZnPercentEncoder >> encode: readStream to: writeStream [
	| bytes buffer byte |
	buffer := (bytes := ByteArray new: 4) writeStream.
	self safeSet; characterEncoder.
	[ readStream atEnd ]
		whileFalse: [ 
			buffer reset.
			characterEncoder nextPut: readStream next toStream: buffer.
			1 to: buffer position do: [ :index | 
				(safeSet includes: (byte := bytes at: index))
					ifTrue: [ writeStream nextPut: byte asCharacter ]
					ifFalse: [ 
						writeStream nextPut: $%.
						self writeHex: byte to: writeStream ] ] ]
]

{ #category : #converting }
ZnPercentEncoder >> errorAsciiCharacterExpected [
	ZnCharacterEncodingError signal: 'ASCII character expected'
]

{ #category : #'error handling' }
ZnPercentEncoder >> errorHexDigitExpected [
	ZnCharacterEncodingError signal: 'hex digit expected'
]

{ #category : #private }
ZnPercentEncoder >> readHexFrom: stream [
	| first second |
	(stream atEnd not and: [ (first := stream next digitValue) between: 0 and: 15 ])
		ifFalse: [ self errorHexDigitExpected ].
	(stream atEnd not and: [ (second := stream next digitValue) between: 0 and: 15 ])
		ifFalse: [ self errorHexDigitExpected ].
	^ (first << 4) + second
]

{ #category : #accessing }
ZnPercentEncoder >> safeSet [
	"Return the safe set of characters that I will not encode, as a byte array.
	If not set, I will default to the most commonly used safe set"
	
	^ safeSet ifNil: [ safeSet := self class rfc3986UnreservedCharacters asByteArray ]
]

{ #category : #'initialize-release' }
ZnPercentEncoder >> safeSet: string [
	"Set my safe set to be the characters in string, which I will convert to bytes"
	
	safeSet := string asByteArray 
]

{ #category : #private }
ZnPercentEncoder >> writeHex: integer to: stream [
	integer printOn: stream base: 16 length: 2 padded: true
]
