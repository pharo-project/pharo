"
This class represents the symbols containing 32bit characters.
"
Class {
	#name : #WideSymbol,
	#superclass : #Symbol,
	#type : #words,
	#category : #'Collections-Strings'
}

{ #category : #converting }
WideSymbol >> asWideString [ 

	^ self.
]

{ #category : #accessing }
WideSymbol >> at: index [ 
	"Answer the Character stored in the field of the receiver indexed by the argument."
	^ Character value: (self wordAt: index).

]

{ #category : #accessing }
WideSymbol >> byteAt: index [

	| d r |
	d := (index + 3) // 4.
	r := (index - 1) \\ 4 + 1.
	^ (self wordAt: d) digitAt: ((4 - r) + 1).

]

{ #category : #accessing }
WideSymbol >> byteAt: index put: aByte [
	^ self modificationForbiddenFor: #byteAt:put: index: index value: aByte
]

{ #category : #accessing }
WideSymbol >> byteSize [

	^ self size * 4.

]

{ #category : #comparing }
WideSymbol >> findIn: body startingAt: start matchTable: matchTable [
	
	^ self findSubstring: self in: body startingAt: start matchTable: matchTable
]

{ #category : #testing }
WideSymbol >> isWideString [
	"Answer whether the receiver is a WideString"
	^true
]

{ #category : #private }
WideSymbol >> privateAt: index put: aCharacter [
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [ (index between: 1 and: self size)
			ifFalse: [ ^ self errorSubscriptBounds: index] ]
		ifFalse: [^ self errorNonIntegerIndex].
	self isReadOnlyObject 
		ifTrue: [ ^ self modificationForbiddenFor: #privateAt:put: index: index value: aCharacter ].

]

{ #category : #private }
WideSymbol >> species [
	"Answer the preferred class for reconstructing the receiver."
	^WideString

]

{ #category : #private }
WideSymbol >> string: aString [
	1 to: aString size do: [:j | 
		self privateAt: j put: (aString at: j) asInteger].
	^ self
]

{ #category : #accessing }
WideSymbol >> wordAt: index [
	<primitive: 60>
	^ (self basicAt: index).

]

{ #category : #accessing }
WideSymbol >> wordAt: index put: anInteger [
	^ self modificationForbiddenFor: #wordAt:put: index: index value: anInteger 
]
