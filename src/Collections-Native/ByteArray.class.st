"
I represent an ArrayedCollection whose elements are integers between 0 and 255.

"
Class {
	#name : #ByteArray,
	#superclass : #ArrayedCollection,
	#type : #bytes,
	#category : #'Collections-Native-Base'
}

{ #category : #'byte based hash' }
ByteArray class >> hashBytes: aByteArray startingWith: speciesHash [
	"Answer the hash of a byte-indexed collection,
	using speciesHash as the initial value.
	See SmallInteger>>hashMultiply.

	The primitive should be renamed at a
	suitable point in the future"

	| byteArraySize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	<var: #aHash declareC: 'int speciesHash'>
	<var: #aByteArray declareC: 'unsigned char *aByteArray'>

	byteArraySize := aByteArray size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: byteArraySize do: [:pos |
		hash := hash + (aByteArray basicAt: pos).
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash
]

{ #category : #'instance creation' }
ByteArray class >> readHexFrom: aString [
	"Create a byte array from a hexadecimal representation"
	^(self new: aString size // 2) readHexFrom: aString readStream
]

{ #category : #converting }
ByteArray >> asByteArray [
	^ self
]

{ #category : #private }
ByteArray >> asByteArrayPointer [
	"Return a ByteArray describing a pointer to the contents of the receiver."
	^self shouldNotImplement
]

{ #category : #converting }
ByteArray >> asString [
	"Convert to a String with Characters for each byte.
	Fast code uses primitive that avoids character conversion"

	^ (String new: self size) replaceFrom: 1 to: self size with: self
]

{ #category : #accessing }
ByteArray >> asWideString [

	^ WideString fromByteArray: self.

]

{ #category : #accessing }
ByteArray >> atAllPut: value [
	"Fill the receiver with the given value"

	<primitive: 145>
	super atAllPut: value
]

{ #category : #accessing }
ByteArray >> byteAt: index [
	<primitive: 60>
	^self at: index
]

{ #category : #accessing }
ByteArray >> byteAt: index put: value [
	<primitive: 61>
	^self at: index put: value
]

{ #category : #accessing }
ByteArray >> byteSize [
	^self size
]

{ #category : #private }
ByteArray >> defaultElement [

	^0
]

{ #category : #'platform independent access' }
ByteArray >> doubleAt: index bigEndian: bool [ 
	"Return a 64 bit float starting from the given byte index"
	| w1 w2 dbl |
	w1 := self unsignedLongAt: index bigEndian: bool.
	w2 := self unsignedLongAt: index + 4 bigEndian: bool.
	dbl := Float new: 2. 
	bool
		ifTrue: [dbl basicAt: 1 put: w1.
			dbl basicAt: 2 put: w2]
		ifFalse: [dbl basicAt: 1 put: w2.
			dbl basicAt: 2 put: w1].
	^ dbl
]

{ #category : #'platform independent access' }
ByteArray >> doubleAt: index put: value bigEndian: bool [ 
	"Store a 64 bit float starting from the given byte index"
	| w1 w2 |
	bool
		ifTrue: [w1 := value basicAt: 1.
			w2 := value basicAt: 2]
		ifFalse: [w1 := value basicAt: 2.
			w2 := value basicAt: 1]. 
	self unsignedLongAt: index put: w1 bigEndian: bool.
	self unsignedLongAt: index + 4 put: w2 bigEndian: bool.
	^ value
]

{ #category : #'platform independent access' }
ByteArray >> floatAt: index bigEndian: boolean [ 
	^ Float
		fromIEEE32Bit: (self unsignedLongAt: index bigEndian: boolean)
]

{ #category : #comparing }
ByteArray >> hash [
	"#hash is implemented, because #= is implemented"

	^self class
		hashBytes: self
		startingWith: self species hash
]

{ #category : #converting }
ByteArray >> hex [
    " an alternate implementation was | result stream |
	result := String new: self size * 2.
	stream := result writeStream.
	1 to: self size do: [ :ix | |each|
		each := self at: ix.
		stream
			nextPut: ('0123456789ABCDEF' at: each // 16 + 1);
			nextPut: ('0123456789ABCDEF' at: each \\ 16 + 1)].
    ^ result"
	
	"Answer a hexa decimal representation of the receiver"
	| string v index map |
	map := '0123456789abcdef'.
	string := String new: self size * 2. "hex"
	index := 0.
	1 to: self size do: [ :i |
		v := self at: i.
		string at: (index := index + 1) put: (map at: (v bitShift: -4) + 1).
		string at: (index := index + 1) put: (map at: (v bitAnd: 15) + 1)].
	^string
]

{ #category : #printing }
ByteArray >> hexDumpOn: aStream max: maxBytes [
	"Print a hex dump of the receiver on the supplied stream, up to maxBytes long"

	| ch i string remainder |

	i := 0.
	self readStreamDo: [ :stream |
		ch := stream next.
		[ch notNil and: [ i < maxBytes ]] whileTrue: [ 
			remainder := i \\ 16.
			remainder = 0 ifTrue: [ 
				i = 0 ifFalse: [ aStream cr ].
				aStream 
					<< (i printPaddedWith: $0 to: 8 base: 16);
					<< '  '.
				string := String new writeStream ].
			aStream
				<< (ch printPaddedWith: $0 to: 2 base: 16);
				<< ' '.
			ch := Character value: ch.
			ch isAlphaNumeric ifTrue: 
				[ string nextPut: ch ]
			ifFalse:
				[ string nextPut: $. ].
			remainder = 15 ifTrue: [ 
				aStream
					<< '  |';
					<< string contents;
					<< '|' ].
			ch := stream next.
			i := i + 1 ].
		remainder := i \\ 16.
		(ch isNil and: [remainder between: 1 and: 14]) ifTrue: [ 
			(16 - remainder) timesRepeat: [ aStream nextPutAll: '   ' ].
			aStream
				<< '  |';
				<< string contents.
			(16 - remainder timesRepeat: [ aStream nextPut: Character space ]).
			aStream
				<< '|' ].
		aStream cr.
		self size > maxBytes ifTrue: 
			[ aStream nextPutAll: '  ... truncated ...'; cr ] ].

]

{ #category : #accessing }
ByteArray >> indexOf: anInteger startingAt: start [

	(anInteger isInteger and: [ anInteger between: 0 and: 255 ]) ifFalse: [ ^0 ].
	^ByteString indexOfAscii: anInteger inString: self startingAt: start
]

{ #category : #testing }
ByteArray >> isLiteral [
	"so that #(1 #[1 2 3] 5) prints itself"
	""
	^ self class == ByteArray
]

{ #category : #'platform independent access' }
ByteArray >> longAt: index bigEndian: aBool [
	"Return a 32bit integer quantity starting from the given byte index"
	| b0 b1 b2 w h |
	aBool ifTrue:[
		b0 := self at: index.
		b1 := self at: index+1.
		b2 := self at: index+2.
		w := self at: index+3.
	] ifFalse:[
		w := self at: index.
		b2 := self at: index+1.
		b1 := self at: index+2.
		b0 := self at: index+3.
	].
	"Minimize LargeInteger arithmetic"
	h := ((b0 bitAnd: 16r7F) - (b0 bitAnd: 16r80) bitShift: 8) + b1.
	b2 = 0 ifFalse:[w := (b2 bitShift: 8) + w].
	h = 0 ifFalse:[w := (h bitShift: 16) + w].
	^w
]

{ #category : #'platform independent access' }
ByteArray >> longAt: index put: value bigEndian: aBool [
	"Return a 32bit integer quantity starting from the given byte index"
	| b0 b1 b2 b3 |
	b0 := value bitShift: -24.
	b0 := (b0 bitAnd: 16r7F) - (b0 bitAnd: 16r80).
	b0 < 0 ifTrue:[b0 := 256 + b0].
	b1 := (value bitShift: -16) bitAnd: 255.
	b2 := (value bitShift: -8) bitAnd: 255.
	b3 := value bitAnd: 255.
	aBool ifTrue:[
		self at: index put: b0.
		self at: index+1 put: b1.
		self at: index+2 put: b2.
		self at: index+3 put: b3.
	] ifFalse:[
		self at: index put: b3.
		self at: index+1 put: b2.
		self at: index+2 put: b1.
		self at: index+3 put: b0.
	].
	^value
]

{ #category : #printing }
ByteArray >> printOn: aStream [

	aStream nextPutAll: '#['.
	self
		do: [ :each | each printOn: aStream ]
		separatedBy: [ aStream nextPut: $ ].
	aStream nextPut: $]
]

{ #category : #'initialize-release' }
ByteArray >> readHexFrom: aStream [
	"Initialize the receiver from a hexadecimal string representation"
	| map v ch value |
	map := '0123456789abcdefABCDEF'.
	1 to: self size do: [ :i |
		ch := aStream next.
		v := (map indexOf: ch) - 1.
		((v between: 0 and: 15) or: [((v:= v - 6) between: 0 and: 15)]) ifFalse:[^self error: 'Hex digit 
expected'].
		value := v bitShift: 4.
		ch := aStream next.
		v := (map indexOf: ch) - 1.
		((v between: 0 and: 15) or: [((v:= v - 6) between: 0 and: 15)]) ifFalse:[^self error: 'Hex digit 
expected'].
		value := value + v.
		self at: i put: value].
]

{ #category : #private }
ByteArray >> replaceFrom: start to: stop with: replacement startingAt: repStart [ 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	replacement isString
		ifFalse:
			[super replaceFrom: start to: stop with: replacement startingAt: repStart]
		ifTrue:
			[ "use String>>byteAt: to mimic prim 105"
			| index repOff |
			repOff := repStart - start.
			index := start - 1.
			[(index := index + 1) <= stop]
				whileTrue: [self at: index put: (replacement byteAt: repOff + index)]]

]

{ #category : #'platform independent access' }
ByteArray >> shortAt: index bigEndian: aBool [
	"Return a 16 bit integer quantity starting from the given byte index"
	| uShort |
	uShort := self unsignedShortAt: index bigEndian: aBool.
	^(uShort bitAnd: 16r7FFF) - (uShort bitAnd: 16r8000)
]

{ #category : #'platform independent access' }
ByteArray >> shortAt: index put: value bigEndian: aBool [
	"Store a 16 bit integer quantity starting from the given byte index"
	self unsignedShortAt: index put: (value bitAnd: 16r7FFF) - (value bitAnd: -16r8000) bigEndian: aBool.
	^value
]

{ #category : #printing }
ByteArray >> storeOn: aStream [
	aStream nextPutAll: '#['.
	self
		do: [ :each | each storeOn: aStream ]
		separatedBy: [ aStream nextPut: $ ].
	aStream nextPut: $]
]

{ #category : #'platform independent access' }
ByteArray >> unsignedLongAt: index bigEndian: aBool [
	"Return a 32bit unsigned integer quantity starting from the given byte index"
	| b0 b1 b2 w |
	aBool ifTrue:[
		b0 := self at: index.
		b1 := self at: index+1.
		b2 := self at: index+2.
		w := self at: index+3.
	] ifFalse:[
		w := self at: index.
		b2 := self at: index+1.
		b1 := self at: index+2.
		b0 := self at: index+3.
	].
	"Minimize LargeInteger arithmetic"
	b2 = 0 ifFalse:[w := (b2 bitShift: 8) + w].
	b1 = 0 ifFalse:[w := (b1 bitShift: 16) + w].
	b0 = 0 ifFalse:[w := (b0 bitShift: 24) + w].
	^w
]

{ #category : #'platform independent access' }
ByteArray >> unsignedLongAt: index put: value bigEndian: aBool [
	"Store a 32bit unsigned integer quantity starting from the given byte index"
	| b0 b1 b2 b3 |
	b0 := value bitShift: -24.
	b1 := (value bitShift: -16) bitAnd: 255.
	b2 := (value bitShift: -8) bitAnd: 255.
	b3 := value bitAnd: 255.
	aBool ifTrue:[
		self at: index put: b0.
		self at: index+1 put: b1.
		self at: index+2 put: b2.
		self at: index+3 put: b3.
	] ifFalse:[
		self at: index put: b3.
		self at: index+1 put: b2.
		self at: index+2 put: b1.
		self at: index+3 put: b0.
	].
	^value
]

{ #category : #'platform independent access' }
ByteArray >> unsignedShortAt: index bigEndian: aBool [
	"Return a 16 bit unsigned integer quantity starting from the given byte index"
	^aBool 
		ifTrue:[((self at: index) bitShift: 8) + (self at: index+1)]
		ifFalse:[((self at: index+1) bitShift: 8) + (self at: index)].
]

{ #category : #'platform independent access' }
ByteArray >> unsignedShortAt: index put: value bigEndian: aBool [
	"Store a 16 bit unsigned integer quantity starting from the given byte index"
	aBool ifTrue:[
		self at: index put: (value bitShift: -8).
		self at: index+1 put: (value bitAnd: 255).
	] ifFalse:[
		self at: index+1 put: (value bitShift: -8).
		self at: index put: (value bitAnd: 255).
	].
	^value
]
