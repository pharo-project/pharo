"
This class represents the array of 32 bit wide characters.

"
Class {
	#name : #WideString,
	#superclass : #String,
	#type : #words,
	#category : #'Collections-Strings'
}

{ #category : #'instance creation' }
WideString class >> from: aString [ 

	| newString |
	(aString isMemberOf: self)
		ifTrue: [^ aString copy].
	newString := self new: aString size.
	1 to: aString size do: [:index | newString basicAt: index put: (aString basicAt: index)].
	^ newString

]

{ #category : #'instance creation' }
WideString class >> fromByteArray: aByteArray [ 

	| inst |
	aByteArray size \\ 4 = 0 ifFalse: [^ ByteString fromByteArray: aByteArray ].
	inst := self new: aByteArray size // 4.
	4 to: aByteArray size by: 4 do: [:i |
		inst basicAt: i // 4
			put: ((aByteArray at: i - 3) << 24) + 
				((aByteArray at: i - 2) << 16) +
				 ((aByteArray at: i - 1) << 8) +
				(aByteArray at: i)
	].

	^ inst

]

{ #category : #'instance creation' }
WideString class >> fromString: aString [ 
	"Answer an instance of me that is a copy of the argument, aString."

	| inst |
	(aString isMemberOf: self) ifTrue: [
		^ aString copy.
	].
	inst := self new: aString size.
	1 to: aString size do: [:pos |
		inst basicAt: pos put: (aString basicAt: pos).
	].
	^ inst.

]

{ #category : #converting }
WideString >> asFourCode [

	| result |
	self size = 1 ifFalse: [^ Error signal: 'must be exactly four octets' ].
	result := self basicAt: 1.
	(result bitAnd: 16r80000000) = 0 
		ifFalse: [ Error signal: 'cannot resolve fourcode' ].
	(result bitAnd: 16r40000000) = 0 
		ifFalse: [ ^ result - 16r80000000 ].
	^ result

]

{ #category : #converting }
WideString >> asLowercase [
	"Answer a copy of self with all characters in a lower case.
	This might be a difficult task, ask Unicode."
	
	^ Unicode toLowercaseString: self
]

{ #category : #converting }
WideString >> asUppercase [
	"Answer a copy of self with all characters in an upper case.
	This might be a difficult task, ask Unicode."
	
	^ Unicode toUppercaseString: self
]

{ #category : #converting }
WideString >> asWideString [ 

	^ self.
]

{ #category : #accessing }
WideString >> at: index [
	"Answer the Character stored in the field of the receiver indexed by the
	 argument.  Primitive.  Fail if the index argument is not an Integer or is out
	 of bounds.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^index isInteger
		ifTrue:
			[self errorSubscriptBounds: index]
		ifFalse:
			[index isNumber
				ifTrue: [self at: index asInteger]
				ifFalse: [self errorNonIntegerIndex]]
]

{ #category : #accessing }
WideString >> at: index put: aCharacter [
	"Store the Character into the field of the receiver indicated by the index.
	 Primitive.  Fail if the index is not an Integer or is out of bounds, or if the
	 argument is not a Character.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter
		ifTrue:
			[index isInteger
				ifTrue: [ ( index between: 1 and: self size )
					ifFalse: [ ^ self errorSubscriptBounds: index ] ]
				ifFalse: [ ^ self errorNonIntegerIndex ]]
		ifFalse:
			[ ^ self errorImproperStore ].
	self isReadOnlyObject 
		ifTrue: [ ^ self modificationForbiddenFor: #at:put: index: index value: aCharacter ].
]

{ #category : #accessing }
WideString >> byteAt: index [

	| d r |
	d := (index + 3) // 4.
	r := (index - 1) \\ 4 + 1.
	^ (self wordAt: d) digitAt: ((4 - r) + 1).

]

{ #category : #accessing }
WideString >> byteAt: index put: aByte [

	| d r w |
	d := (index + 3) // 4.
	r := (index - 1) \\ 4 + 1.
	w := (self wordAt: d) bitAnd: ((16rFF<<((4 - r)*8)) bitInvert32).
	w := w + (aByte<<((4 - r)*8)).
	self basicAt: d put: w.
	^ aByte.

]

{ #category : #accessing }
WideString >> byteSize [

	^ self size * 4.

]

{ #category : #converting }
WideString >> copyFrom: start to: stop [

	| n |
	n := super copyFrom: start to: stop.
	n isOctetString ifTrue: [^ n asOctetString].
	^ n.

]

{ #category : #comparing }
WideString >> findIn: body startingAt: start matchTable: matchTable [
	
	^ self findSubstring: self in: body startingAt: start matchTable: matchTable
]

{ #category : #testing }
WideString >> isWideString [
	"Answer whether the receiver is a WideString"
	^true
]

{ #category : #accessing }
WideString >> replaceFrom: start to: stop with: replacement startingAt: repStart [ 

	<primitive: 105>
	replacement class == String ifTrue: [
		^ self replaceFrom: start to: stop with: (replacement asWideString) startingAt: repStart.
	]. 

	^ super replaceFrom: start to: stop with: replacement startingAt: repStart.

]

{ #category : #accessing }
WideString >> wordAt: index [
	<primitive: 60>
	^ (self basicAt: index).

]

{ #category : #accessing }
WideString >> wordAt: index put: anInteger [
	<primitive: 61>
	self basicAt: index put: anInteger.

]
