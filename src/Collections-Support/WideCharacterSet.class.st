"
WideCharacterSet is used to store a Set of WideCharacter with fast access and inclusion test.

Implementation should be efficient in memory if sets are sufficently sparse.

Wide Characters are at most 32bits.
We split them into 16 highBits and 16 lowBits.

map is a dictionary key: 16 highBits value: map of 16 lowBits.

Maps of lowBits  are stored as arrays of bits in a ByteArray.
If a bit is set to 1, this indicate that corresponding character is present.
8192 bytes are necessary in each lowmap.
Empty lowmap are removed from the map Dictionary.

A byteArrayMap is maintained in parallel with map for fast handling of ByteString.
(byteArrayMap at: i+1) = 0 means that character of asciiValue i is absent, = 1 means present.
"
Class {
	#name : #WideCharacterSet,
	#superclass : #CharacterSet,
	#instVars : [
		'byteArrayMap'
	],
	#category : #'Collections-Support'
}

{ #category : #'instance creation' }
WideCharacterSet class >> new: initialSize [
	"ignore the initial size"
	^ self new
]

{ #category : #'instance creation' }
WideCharacterSet class >> newFrom: aCollection [
	| newCollection |
	newCollection := self new.
	newCollection addAll: aCollection.
	^newCollection
]

{ #category : #comparing }
WideCharacterSet >> = anObject [
	self class == anObject class
		ifTrue: [
			^ (self byteArrayMap = anObject byteArrayMap) and: [ 
				self wideCharacterMap = anObject wideCharacterMap ] ].
	((anObject respondsTo: #size) and: [ anObject respondsTo: #includes: ])
		ifFalse: [ ^ false ].
	self size = anObject size
		ifFalse: [ ^ false ].
	^ self allSatisfy: [ :each | (anObject includes: each)].
]

{ #category : #'collection ops' }
WideCharacterSet >> add: aCharacter [ 
	| val high low lowmap |
	val := aCharacter asciiValue.
	val < 256 ifTrue: [
		byteArrayMap at: val + 1 put: 1.
		^ aCharacter ].
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	lowmap := map at: high ifAbsentPut: ["create a chunk of 65536=8192*8 bits"
		ByteArray new: 8192].
	self setBitmap: lowmap at: low.
	^ aCharacter
]

{ #category : #private }
WideCharacterSet >> bitmap: aMap at: shortInteger [
	"access a single bit in aMap.
	shortInteger should be between: 0 and: 16rFFFF"
	
	| collecIndex bitIndex |
	collecIndex := shortInteger bitShift: -3.
	bitIndex := shortInteger bitAnd: 7.
	^(aMap at: collecIndex + 1) bitAnd: (1 bitShift: bitIndex)
]

{ #category : #private }
WideCharacterSet >> bitmap: aMap do: aBlock [
	"Execute a block with each value (0 based) corresponding to set bits.
	Implementation notes: this version works best for sparse maps.
	It has (byte lowBit) inlined for speed."
	
	| byte byteOffset lowBits |
	lowBits := #[1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 6 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 7 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 6 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 8 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 6 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 7 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 6 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1]. "The lowBits table gives a 1-based bitOffset"
	1 to: aMap size do: [:i | 
		(byte := aMap at: i) = 0 ifFalse: [
			byteOffset := (i bitShift: 3) - 9. "This byteOffset is -1 based"
			["Evaluate the block with 0-based (byteOffset + bitOffset)"
			aBlock value: (byteOffset + (lowBits at: byte)).
			"Eliminate the low bit and loop if some bit remain"
			(byte := byte bitAnd: byte - 1) = 0] whileFalse]]
]

{ #category : #comparing }
WideCharacterSet >> byteArrayMap [
	"returns a ByteArray mapping n < 256 to 1 if (Character asciValue: (n-1)) is in the set, 
	and  0 if it isn't.
	Intended for internal use, and for comparison."
	
	^ byteArrayMap
]

{ #category : #private }
WideCharacterSet >> clearBitmap: aMap at: shortInteger ifNotSet: anAction [
	"clear a single bit in aMap.
	shortInteger should be between: 0 and: 16rFFFF"

	| collecIndex oldMapEntry bitMask |
	collecIndex := (shortInteger bitShift: -3) + 1.
	oldMapEntry := aMap at: collecIndex.
	bitMask := 1 bitShift: (shortInteger bitAnd: 7).
	(oldMapEntry bitAnd: bitMask) = 0
		ifTrue: [ ^ anAction value ].
	^ aMap at: collecIndex put: (oldMapEntry bitClear: bitMask)
]

{ #category : #enumerating }
WideCharacterSet >> do: aBlock [ 
	byteArrayMap withIndexDo: [ :each :index | (each = 1) 
			ifTrue: [ aBlock value: (Character value: index - 1) ]].
	map
		keysAndValuesDo: [:index :lowmap |
			| high16Bits |
			high16Bits := index bitShift: 16.
			self
				bitmap: lowmap
				do: [:low16Bits | aBlock value: (Character value: high16Bits + low16Bits)]]
]

{ #category : #enumerating }
WideCharacterSet >> findFirstInByteString: aByteString startingAt: startIndex [
	"Double dispatching: since we know first argument is a ByteString, 
	we can use a ByteString primitive."
	
	^ ByteString
		findFirstInString: aByteString
		inSet: byteArrayMap
		startingAt: startIndex
]

{ #category : #testing }
WideCharacterSet >> hasWideCharacters [
	"Answer true if i contain any wide character"
	
	self do: [:e | e asciiValue >= 256 ifTrue: [^true]].
	^false
]

{ #category : #comparing }
WideCharacterSet >> hash [
	"Answer a hash code aimed at storing and retrieving the receiver in a Set or Dictionary.
	Two equal objects should have equal hash.
	Note: as the receiver can be equal to an ordinary CharacterSet,
	the hash code must reflect this"
	
	^self hasWideCharacters
		ifTrue: [map hash]
		ifFalse: [self asCharacterSet hash]
]

{ #category : #private }
WideCharacterSet >> includes: aCharacter [ 
	| val high low |
	val := aCharacter asciiValue.
	val < 256 ifTrue: [
		^ (self byteArrayMap at: val + 1) = 1
	].
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	^(self
		bitmap: (map
				at: high
				ifAbsent: [^ false])
		at: low) isZero not
]

{ #category : #initialization }
WideCharacterSet >> initialize [
	super initialize.
	map := Dictionary new.
	byteArrayMap := ByteArray new: 256
]

{ #category : #accessing }
WideCharacterSet >> like: anObject [
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties.
	Note, use #like:ifAbsent: if you need to match against nil as element"

	^ self like: anObject ifAbsent: [ nil ]
]

{ #category : #accessing }
WideCharacterSet >> like: aCharacter ifAbsent: aBlock [
	"Answer an object in the receiver that is equal to anObject,
	or evaluate the block if not found. Relies heavily on hash properties"

	^ (self includes: aCharacter)
		ifTrue: [ aCharacter ]
		ifFalse: [ aBlock value ]
]

{ #category : #copying }
WideCharacterSet >> postCopy [
	super postCopy.
	byteArrayMap := byteArrayMap copy.
	map := map collect: [:each | each copy]
]

{ #category : #removing }
WideCharacterSet >> remove: aCharacter ifAbsent: aBlock [
	| val high low lowmap |
	val := aCharacter asciiValue.
	val < 256 ifTrue: [
		| index |
		index := val + 1.
		((byteArrayMap at: index) = 0) ifTrue: [ ^ aBlock value ].
		byteArrayMap at: index put: 0.
		^ aCharacter
	].
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	lowmap := map
				at: high
				ifAbsent: [^ aBlock value].
	self clearBitmap: lowmap at: low ifNotSet: [^ aBlock value].
	(lowmap allSatisfy: [:e | e = 0])
		ifTrue: [map removeKey: high].
	^ aCharacter
]

{ #category : #'collection ops' }
WideCharacterSet >> removeAll [
	map removeAll.
	byteArrayMap := ByteArray new: 256
]

{ #category : #private }
WideCharacterSet >> setBitmap: aMap at: shortInteger [
	"set a single bit in aMap.
	shortInteger should be between: 0 and: 16rFFFF"
	
	| collecIndex bitIndex |
	collecIndex := shortInteger bitShift: -3.
	bitIndex := shortInteger bitAnd: 7.
	^aMap at: collecIndex + 1 put: ((aMap at: collecIndex + 1) bitOr: (1 bitShift: bitIndex))
]

{ #category : #accessing }
WideCharacterSet >> size [
	| size |
	size := 0.
	self do: [ :_ | size := size + 1].
	^ size
]

{ #category : #private }
WideCharacterSet >> species [
	^self hasWideCharacters
		ifTrue: [WideCharacterSet]
		ifFalse: [CharacterSet]
]

{ #category : #private }
WideCharacterSet >> wideCharacterMap [
	^map
]
