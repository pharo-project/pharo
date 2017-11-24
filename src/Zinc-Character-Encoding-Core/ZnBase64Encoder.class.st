"
I am ZnBase64Encoder.

Base64 encoding is a technique to encode binary data as a string of characters that can be safely transported over various protocols. Basically, every 3 bytes are encoded using 4 characters from an alphabet of 64. Each encoded character represents 6 bits.

The most commonly used alphabet is 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'. One or two equal signs (= or ==) are used for padding.

  ZnBase64Encoder new encode: #[0 1 2 3 4 5].
  ZnBase64Encoder new encode: #[10 20]
  ZnBase64Encoder new decode: 'BQQDAgEA'.
  ZnBase64Encoder new decode: 'FAo='.

The encoded data can optionally be broken into lines. Characters not part of the alphabet are considered as white space and are ignored when inbetween groups of 4 characters.

My #encode: method works from ByteArray to String, while my #decode: method works from String to ByteArray.

Note that to encode a String as Base64, you first have to encode the characters as bytes using a character encoder.

See also http://en.wikipedia.org/wiki/Base64

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnBase64Encoder,
	#superclass : #Object,
	#instVars : [
		'alphabet',
		'inverse',
		'lineLength',
		'lineEnd',
		'whitespace'
	],
	#classVars : [
		'DefaultAlphabet',
		'DefaultInverse'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #'class initialization' }
ZnBase64Encoder class >> initialize [
	DefaultAlphabet := String withAll: ($A to: $Z) , ($a to: $z) , ($0 to: $9) , #($+ $/).
	DefaultInverse := Array new: 128.
	0 to: 127 do: [ :each | 
		| offset |
		offset := DefaultAlphabet indexOf: each asCharacter ifAbsent: [ nil ].
		DefaultInverse at: each + 1 put: (offset ifNotNil: [ offset - 1 ]) ]
]

{ #category : #accessing }
ZnBase64Encoder >> alphabet [
	"Return the alphabet that I am using to encode byte values"
	
	^ alphabet 
]

{ #category : #initialization }
ZnBase64Encoder >> alphabet: string [
	"Set the alphabet to use to string, containing 64 characters to represent 64 byte values.
	I automatically compute the inverse used for fast decoding."
	
	self assert: string size = 64 description: '64 characters are needed for a Base64 alphabet'.
	alphabet := string.
	inverse := Array new: 128.
	0 to: 127 do: [ :each | 
		| offset |
		offset := alphabet indexOf: each asCharacter ifAbsent: [ nil ].
		inverse at: each + 1 put: (offset ifNotNil: [ offset - 1 ]) ]
]

{ #category : #initialization }
ZnBase64Encoder >> breakLines [
	"Configure me to break lines and insert newlines every 76 characters while encoding"

	self breakLinesAt: 76
]

{ #category : #initialization }
ZnBase64Encoder >> breakLinesAt: length [
	"Configure me to break lines at lenth, a multiple of 4, and insert newlines"

	self assert: (length \\ 4) isZero description: 'line length should be a multiple of 4'.
	lineLength := length.
	lineEnd ifNil: [ self lineEndConvention: #crlf ]
]

{ #category : #private }
ZnBase64Encoder >> byteCountFor: string [	
	| n byteCount |
	"This assumes there are no line breaks in string"
	n := string size.
	byteCount := n // 4 * 3.
	(n > 1 and: [ (string at: n) = $= ])
		ifTrue: [ 
			(n > 2 and: [ (string at: n - 1) = $= ])
				ifTrue: [ byteCount := byteCount - 2 ]
				ifFalse: [ byteCount := byteCount - 1 ] ].
	^ byteCount
]

{ #category : #private }
ZnBase64Encoder >> characterCountFor: bytes [
	| n characterCount | 
	n := bytes size.
	characterCount := n // 3 + (n \\ 3) sign * 4.
	^ lineLength isNil
		ifTrue: [ characterCount ]
		ifFalse: [ characterCount + ((characterCount // lineLength ) * lineEnd size) ]
]

{ #category : #private }
ZnBase64Encoder >> characterForValue: value [
	^ alphabet at: value + 1
]

{ #category : #converting }
ZnBase64Encoder >> decode: string [
	"Decode a Base64 encoded string and return the resulting byte array"
	
	^ ByteArray 
		new: (self byteCountFor: string)
		streamContents: [ :byteStream | 
			self decode: string readStream to: byteStream ]
]

{ #category : #private }
ZnBase64Encoder >> decode: char1 and: char2 and: char3 and: char4 to: stream [
	| v1 v2 v3 v4 |
	v1 := self valueForCharacter: char1.
	v2 := self valueForCharacter: char2.
	stream nextPut: (v1 bitShift: 2) + (v2 bitShift: -4).
	char3 == $=
		ifFalse: [ 
			v3 := self valueForCharacter: char3.
			stream nextPut: ((v2 bitAnd: 2r1111) bitShift: 4) + (v3 bitShift: -2).
			char4 == $=
				ifFalse: [ 
						v4 := self valueForCharacter: char4.
						stream nextPut: ((v3 bitAnd: 2r11) bitShift: 6) + v4 ] ]
]

{ #category : #converting }
ZnBase64Encoder >> decode: stringStream to: byteStream [
	| char1 char2 char3 char4 |
	[ stringStream atEnd ] whileFalse: [ 
		self skipWhitespace: stringStream.
		stringStream atEnd ifTrue: [ ^ self ].
		char1 := stringStream next.
		char2 := stringStream next.
		char3 := stringStream next.
		char4 := stringStream next.
		char1 isNil | char2 isNil | char3 isNil | char4 isNil
			ifTrue: [ ZnCharacterEncodingError signal: 'Illegal Base64 input' ].
		self decode: char1 and: char2 and: char3 and: char4 to: byteStream ]
]

{ #category : #converting }
ZnBase64Encoder >> encode: byteArray [
	"Encode byteArray using Base64 encoding and return the resulting string"
	
	^ String 
		new: (self characterCountFor: byteArray) 
		streamContents: [ :stringStream | 
			self encode: byteArray readStream to: stringStream ]
]

{ #category : #private }
ZnBase64Encoder >> encode: byte1 and: byte2 and: byte3 to: stream [
	stream nextPut: (self characterForValue: (byte1 bitShift: -2)).
	byte2
		ifNil: [ 
			stream nextPut: (self characterForValue: ((byte1 bitAnd: 2r11) bitShift: 4)).
			stream nextPutAll: '==' ]
		ifNotNil: [ 
			stream nextPut: (self characterForValue: (((byte1 bitAnd: 2r11) bitShift: 4) + (byte2 bitShift: -4))).
			byte3
				ifNil: [ 
					stream nextPut: (self characterForValue: ((byte2 bitAnd: 2r1111) bitShift: 2)).
					stream nextPut: $= ]
				ifNotNil: [ 
					stream nextPut: (self characterForValue: (((byte2 bitAnd: 2r1111) bitShift: 2) + (byte3 bitShift: -6))).
					stream nextPut: (self characterForValue: (byte3 bitAnd: 2r111111)) ] ]
]

{ #category : #converting }
ZnBase64Encoder >> encode: byteStream to: stringStream [
	| byte1 byte2 byte3 count |
	lineLength
		ifNil: [ 
			[ byteStream atEnd ] whileFalse: [ 
				byte1 := byteStream next.
				byte2 := byteStream next.
				byte3 := byteStream next.
				self encode: byte1 and: byte2 and: byte3 to: stringStream ] ]
		ifNotNil: [ 
			count := 0.
			[ byteStream atEnd ] whileFalse: [ 
				byte1 := byteStream next.
				byte2 := byteStream next.
				byte3 := byteStream next.
				self encode: byte1 and: byte2 and: byte3 to: stringStream.
				(count := count + 4) = lineLength
					ifTrue: [ 
						stringStream nextPutAll: lineEnd.
						count := 0 ] ] ]
]

{ #category : #initialization }
ZnBase64Encoder >> initialize [
	super initialize.
	alphabet := DefaultAlphabet.
	inverse := DefaultInverse.
	self whitespace: #any
]

{ #category : #private }
ZnBase64Encoder >> isLegalCharacter: character [
	"Return true when character is part of my alphabet"

	| code |
	^ (code := character asciiValue) < 128 
			and: [ (inverse at: code + 1) notNil ]
]

{ #category : #private }
ZnBase64Encoder >> isWhitespaceCharacter: character [
	"Return true when character should be considered whitespace"

	whitespace 
		ifNil: [ "No whitespace allowed" 
			^ false ].
	whitespace = #separator
		ifTrue: [ "Only separators are considered whitespace" 
			^ character isSeparator ].
	whitespace = #any
		ifTrue: [ "All non-legal (non-alphabet) characters are considered whitespace" 
			^ (self isLegalCharacter: character) not ].
	^ false
]

{ #category : #initialization }
ZnBase64Encoder >> lineEndConvention: symbol [
	"Set the end of line convention to be used.
	Either #cr, #lf or #crlf (the default)."
	
	self assert: (#(cr lf crlf) includes: symbol).
	lineEnd := String perform: symbol
]

{ #category : #private }
ZnBase64Encoder >> skipWhitespace: stream [
	[ stream atEnd not and: [ (self isWhitespaceCharacter: stream peek) ] ] 
		whileTrue: [ stream next ]
]

{ #category : #private }
ZnBase64Encoder >> valueForCharacter: char [
	| code |
	(code := char asciiValue) < 128
		ifTrue: [ 
			(inverse at: code + 1)
				ifNotNil: [ :byteValue | ^ byteValue ] ].
	ZnCharacterEncodingError signal: 'Illegal Base64 input'
]

{ #category : #initialization }
ZnBase64Encoder >> whitespace: mode [
	"Set the whitespace mode: 
	nil is no whitespace allowed, 
	#separator is CR, LF, FF, SPACE, TAB allowed,
	#any is all non-alphabet characters allowed (the default)"
	
	self assert: (#(nil separator any) includes: mode).
	whitespace := mode
]
