"
This class represents the array of 8 bit wide characters.

"
Class {
	#name : #ByteString,
	#superclass : #String,
	#type : #bytes,
	#classVars : [
		'NonAsciiMap'
	],
	#category : #'Collections-Strings'
}

{ #category : #primitives }
ByteString class >> compare: string1 with: string2 collated: order [
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	| len1 len2 c1 c2 |
	<primitive: 'primitiveCompareString' module: 'MiscPrimitivePlugin'>
	
	<var: #string1 declareC: 'unsigned char *string1'>
	<var: #string2 declareC: 'unsigned char *string2'>
	<var: #order declareC: 'unsigned char *order'>

	len1 := string1 size.
	len2 := string2 size.
	1 to: (len1 min: len2) do:
		[:i |
		c1 := order at: (string1 basicAt: i) + 1.
		c2 := order at: (string2 basicAt: i) + 1.
		c1 = c2 ifFalse: 
			[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].

]

{ #category : #primitives }
ByteString class >> findFirstInString: aString  inSet: inclusionMap  startingAt: start [
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>
	<var: #aString declareC: 'unsigned char *aString'>
	<var: #inclusionMap  declareC: 'char *inclusionMap'>

	inclusionMap size ~= 256 ifTrue: [ ^0 ].

	i := start.
	stringSize := aString size.
	[ i <= stringSize and: [ (inclusionMap at: (aString basicAt: i) + 1) = 0 ] ] whileTrue: [ 
		i := i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i
]

{ #category : #primitives }
ByteString class >> indexOfAscii: anInteger inString: aString startingAt: start [

	| stringSize |
	<primitive: 'primitiveIndexOfAsciiInString' module: 'MiscPrimitivePlugin'>
	<var: #aCharacter declareC: 'int anInteger'>
	<var: #aString declareC: 'unsigned char *aString'>

	stringSize := aString size.
	start to: stringSize do: [:pos |
		(aString basicAt: pos) = anInteger ifTrue: [^ pos]].

	^ 0
]

{ #category : #initialization }
ByteString class >> initialize [
	"ByteString initialize"

	NonAsciiMap := ByteArray new: 256.
	0 to: 255 do: [ :i | 
		i < 128
			ifTrue: [ NonAsciiMap at: i + 1 put: 0	"valid ascii subset" ]
			ifFalse: [ NonAsciiMap at: i + 1 put: 1	"extended charset" ] ]
]

{ #category : #contants }
ByteString class >> nonAsciiMap [
	^NonAsciiMap
]

{ #category : #primitives }
ByteString class >> stringHash: aString initialHash: speciesHash [

	| stringSize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	<var: #aHash declareC: 'int speciesHash'>
	<var: #aString declareC: 'unsigned char *aString'>

	stringSize := aString size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: stringSize do: [:pos |
		hash := hash + (aString basicAt: pos).
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash
]

{ #category : #primitives }
ByteString class >> translate: aString from: start  to: stop  table: table [
	"translate the characters in the string by the given table, in place"
	<primitive: 'primitiveTranslateStringWithTable' module: 'MiscPrimitivePlugin'>
	<var: #table  declareC: 'unsigned char *table'>
	<var: #aString  declareC: 'unsigned char *aString'>

	start to: stop do: [ :i |
		aString at: i put: (table at: (aString basicAt: i) + 1) ]
]

{ #category : #converting }
ByteString >> asByteArray [
	| ba sz |
	sz := self byteSize.
	ba := ByteArray new: sz.
	ba replaceFrom: 1 to: sz with: self startingAt: 1.
	^ba
]

{ #category : #converting }
ByteString >> asOctetString [

	^ self.

]

{ #category : #accessing }
ByteString >> at: index [ 
	"Primitive. Answer the Character stored in the field of the receiver
	indexed by the argument. Fail if the index argument is not an Integer or
	is out of bounds. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^ Character value: (super at: index)
]

{ #category : #accessing }
ByteString >> at: index put: aCharacter [
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter 
		ifFalse:[^self errorImproperStore].
	aCharacter isOctetCharacter ifFalse:[
		"Convert to WideString"
		self becomeForward: (WideString from: self).
		^self at: index put: aCharacter.
	].
	index isInteger
		ifTrue: [ (index between: 1 and: self size)
				ifFalse: [ self errorSubscriptBounds: index ] ]
		ifFalse: [self errorNonIntegerIndex].
	self isReadOnlyObject 
		ifTrue: [ ^ self modificationForbiddenFor: #at:put: index: index value: aCharacter ].

]

{ #category : #comparing }
ByteString >> beginsWith: prefix [
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
ByteString >> byteAt: index [
	<primitive: 60>
	^(self at: index) asciiValue
]

{ #category : #accessing }
ByteString >> byteAt: index put: value [
	<primitive: 61>
	self at: index put: value asCharacter.
	^value
]

{ #category : #accessing }
ByteString >> byteSize [
	^self size
]

{ #category : #comparing }
ByteString >> findSubstring: key in: body startingAt: start matchTable: matchTable [
	
	^ key findIn: body startingAt: start matchTable: matchTable.
]

{ #category : #testing }
ByteString >> hasWideCharacterFrom: start to: stop [
	"Only WideStrings contain these characters"
	^false
]

{ #category : #accessing }
ByteString >> indexOfAnyOf: aCollection startingAt: start [
	"Use double dispatching for speed"
	^aCollection findFirstInByteString: self startingAt: start
]

{ #category : #accessing }
ByteString >> indexOfAnyOf: aCollection startingAt: start ifAbsent: aBlock [
	"Use double dispatching for speed"
	| index |
	^(index := aCollection findFirstInByteString: self startingAt: start) = 0
		ifTrue: [aBlock value]
		ifFalse: [index]
]

{ #category : #testing }
ByteString >> isAsciiString [


 ^(self class findFirstInString: self inSet: NonAsciiMap startingAt: 1) = 0
]

{ #category : #testing }
ByteString >> isByteString [
	"Answer whether the receiver is a ByteString"
	^true
]

{ #category : #testing }
ByteString >> isOctetString [
	"Answer whether the receiver can be represented as a byte string. 
	This is different from asking whether the receiver *is* a ByteString 
	(i.e., #isByteString)"
	^ true.

]

{ #category : #accessing }
ByteString >> replaceFrom: start to: stop with: replacement startingAt: repStart [ 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	replacement class == WideString ifTrue: [
		self becomeForward: (WideString from: self).
	]. 

	super replaceFrom: start to: stop with: replacement startingAt: repStart.

]
