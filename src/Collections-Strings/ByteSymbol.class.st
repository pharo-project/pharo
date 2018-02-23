"
This class represents the symbols containing 8bit characters.
"
Class {
	#name : #ByteSymbol,
	#superclass : #Symbol,
	#type : #bytes,
	#category : #'Collections-Strings'
}

{ #category : #primitives }
ByteSymbol class >> findFirstInString: aString inSet: inclusionMap startingAt: start [
	^ByteString findFirstInString: aString  inSet: inclusionMap startingAt: start
]

{ #category : #primitives }
ByteSymbol class >> indexOfAscii: anInteger inString: aString startingAt: start [
	^ByteString indexOfAscii: anInteger inString: aString startingAt: start
]

{ #category : #primitives }
ByteSymbol class >> stringHash: aString initialHash: speciesHash [
	^ByteString stringHash: aString initialHash: speciesHash
]

{ #category : #primitives }
ByteSymbol class >> translate: aString from: start  to: stop  table: table [
	^ByteString translate: aString from: start  to: stop  table: table
]

{ #category : #converting }
ByteSymbol >> asByteArray [
	| ba sz |
	sz := self byteSize.
	ba := ByteArray new: sz.
	ba replaceFrom: 1 to: sz with: self startingAt: 1.
	^ba
]

{ #category : #converting }
ByteSymbol >> asOctetString [
	^ self
]

{ #category : #accessing }
ByteSymbol >> at: index [ 
	"Primitive. Answer the Character stored in the field of the receiver
	indexed by the argument. Fail if the index argument is not an Integer or
	is out of bounds. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^ Character value: (super at: index)
]

{ #category : #comparing }
ByteSymbol >> beginsWith: prefix [
	"Answer whether the receiver begins with the given prefix string.
	The comparison is case-sensitive."


	"IMPLEMENTATION NOTE:
	following algorithm is optimized in primitive only in case self and prefix are bytes like.
	Otherwise, if self is wide, then super outperforms,
	Otherwise, if prefix is wide, primitive is not correct"
	
	prefix class isBytes ifFalse: [^super beginsWith: prefix].
	
	self size < prefix size ifTrue: [^ false].
	^ (self findSubstring: prefix in: self startingAt: 1
			matchTable: CaseSensitiveOrder) = 1

]

{ #category : #accessing }
ByteSymbol >> byteAt: index [
	<primitive: 60>
	^(self at: index) asciiValue
]

{ #category : #accessing }
ByteSymbol >> byteAt: anInteger put: anObject [ 
	"You cannot modify the receiver."
	^ self modificationForbiddenFor: #byteAt:put: index: anInteger value: anObject
]

{ #category : #accessing }
ByteSymbol >> byteSize [
	^self size
]

{ #category : #comparing }
ByteSymbol >> findSubstring: key in: body startingAt: start matchTable: matchTable [
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned."
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>
	^super findSubstring: key in: body startingAt: start matchTable: matchTable
]

{ #category : #testing }
ByteSymbol >> hasWideCharacterFrom: start to: stop [
	"Always false because I only contains byte characters"
	^false
]

{ #category : #accessing }
ByteSymbol >> indexOfAnyOf: aCollection startingAt: start [
	"Use double dispatching for speed"
	^aCollection findFirstInByteString: self startingAt: start
]

{ #category : #accessing }
ByteSymbol >> indexOfAnyOf: aCollection startingAt: start ifAbsent: aBlock [
	"Use double dispatching for speed"
	| index |
	^(index := aCollection findFirstInByteString: self startingAt: start) = 0
		ifTrue: [aBlock value]
		ifFalse: [index]
]

{ #category : #testing }
ByteSymbol >> isByteString [
	"Answer whether the receiver is a ByteString"
	^true
]

{ #category : #testing }
ByteSymbol >> isOctetString [
	"Answer whether the receiver can be represented as a byte string. 
	This is different from asking whether the receiver *is* a ByteString 
	(i.e., #isByteString)"
	^ true.

]

{ #category : #private }
ByteSymbol >> privateAt: index put: aCharacter [
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter 
		ifFalse:[^self errorImproperStore].
	index isInteger
		ifTrue: [ (index between: 1 and: self size)
				ifFalse: [ self errorSubscriptBounds: index] ]
		ifFalse: [self errorNonIntegerIndex].
	self isReadOnlyObject 
		ifTrue: [ ^ self modificationForbiddenFor: #privateAt:put: index: index value: aCharacter ].

]

{ #category : #accessing }
ByteSymbol >> species [
	"Answer the preferred class for reconstructing the receiver."
	^ByteString

]

{ #category : #private }
ByteSymbol >> string: aString [
	1 to: aString size do: [:j | 
		self privateAt: j put: (aString at: j)].
	^self
]
