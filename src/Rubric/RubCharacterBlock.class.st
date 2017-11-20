Class {
	#name : #RubCharacterBlock,
	#superclass : #Object,
	#instVars : [
		'surface',
		'stringIndex',
		'textLine'
	],
	#category : #Rubric-TextScanning
}

{ #category : #comparing }
RubCharacterBlock >> < aCharacterBlock [ 
	"Answer whether the string index of the receiver precedes that of 
	aCharacterBlock."

	^stringIndex < aCharacterBlock stringIndex
]

{ #category : #comparing }
RubCharacterBlock >> <= aCharacterBlock [ 
	"Answer whether the string index of the receiver does not come after that 
	of aCharacterBlock."

	^(self > aCharacterBlock) not
]

{ #category : #comparing }
RubCharacterBlock >> = aCharacterBlock [

	self species = aCharacterBlock species
		ifTrue: [^stringIndex = aCharacterBlock stringIndex]
		ifFalse: [^false]
]

{ #category : #comparing }
RubCharacterBlock >> > aCharacterBlock [ 
	"Answer whether the string index of the receiver comes after that of 
	aCharacterBlock."

	^aCharacterBlock < self
]

{ #category : #comparing }
RubCharacterBlock >> >= aCharacterBlock [ 
	"Answer whether the string index of the receiver does not precede that of 
	aCharacterBlock."

	^(self < aCharacterBlock) not
]

{ #category : #accessing }
RubCharacterBlock >> area [
	^ surface area
]

{ #category : #accessing }
RubCharacterBlock >> areasOutside: aRectangle [
	^ surface areasOutside: aRectangle
]

{ #category : #accessing }
RubCharacterBlock >> bottom [
	^ surface bottom
]

{ #category : #accessing }
RubCharacterBlock >> bottomLeft [
	^ surface bottomLeft
]

{ #category : #accessing }
RubCharacterBlock >> bottomRight [
	^ surface bottomRight 
]

{ #category : #accessing }
RubCharacterBlock >> corner [
	^ surface corner
]

{ #category : #accessing }
RubCharacterBlock >> distance: aCharacterBlock [
	"It returns absolute distance between two CharacterBlocks."
	^ (self stringIndex - aCharacterBlock stringIndex) abs
]

{ #category : #accessing }
RubCharacterBlock >> extent [
	^ surface extent
]

{ #category : #accessing }
RubCharacterBlock >> height [
	^ surface height
]

{ #category : #private }
RubCharacterBlock >> insetBy: aPoint [ 
	"Change the corner positions of the receiver so that its area translates by 
	the amount defined by the argument, aPoint."
	^ self copy surface: (surface insetBy: aPoint)
]

{ #category : #accessing }
RubCharacterBlock >> left [
	^ surface left
]

{ #category : #comparing }
RubCharacterBlock >> max: aCharacterBlock [
	aCharacterBlock ifNil: [ ^ self ].
	^ aCharacterBlock > self
		ifTrue: [ aCharacterBlock ]
		ifFalse: [ self ]
]

{ #category : #comparing }
RubCharacterBlock >> min: aCharacterBlock [
	aCharacterBlock ifNil: [ ^ self ].
	^ aCharacterBlock < self
		ifTrue: [ aCharacterBlock ]
		ifFalse: [ self ]
]

{ #category : #private }
RubCharacterBlock >> moveBy: aPoint [ 
	"Change the corner positions of the receiver so that its area translates by 
	the amount defined by the argument, aPoint."
	surface := surface translateBy: aPoint
]

{ #category : #accessing }
RubCharacterBlock >> origin [
	^ surface origin
]

{ #category : #printing }
RubCharacterBlock >> printOn: aStream [
	aStream nextPutAll: 'a CharacterBlock with index '.
	stringIndex printOn: aStream.
	aStream nextPutAll: ' and surface '.
	self surface printOn: aStream.
	textLine
		ifNotNil: [ 
			aStream
				cr;
				nextPutAll: ' in '.
			textLine printOn: aStream ]
]

{ #category : #private }
RubCharacterBlock >> privateSetCorner: aPoint [
	self surface privateSetCorner: aPoint
]

{ #category : #private }
RubCharacterBlock >> privateSetOrigin: aPoint [
	self surface privateSetOrigin: aPoint
]

{ #category : #accessing }
RubCharacterBlock >> right [
	^ surface right
]

{ #category : #accessing }
RubCharacterBlock >> stringIndex [
	"Answer the position of the receiver in the string it indexes."

	^stringIndex
]

{ #category : #private }
RubCharacterBlock >> stringIndex: anInteger topLeft: topLeft extent: extent [ 
	stringIndex := anInteger.
	surface := topLeft extent: extent

]

{ #category : #accessing }
RubCharacterBlock >> surface [
	^ surface
]

{ #category : #accessing }
RubCharacterBlock >> surface: aRectangle [
	surface := aRectangle
]

{ #category : #accessing }
RubCharacterBlock >> textLine [
	^ textLine
]

{ #category : #accessing }
RubCharacterBlock >> textLine: aLine [ 
	textLine := aLine
]

{ #category : #accessing }
RubCharacterBlock >> top [
	^ surface top
]

{ #category : #accessing }
RubCharacterBlock >> topLeft [
	^ surface topLeft
]

{ #category : #accessing }
RubCharacterBlock >> topRight [
	^ surface topRight
]

{ #category : #private }
RubCharacterBlock >> translateBy: aPoint [ 
	"Change the corner positions of the receiver so that its area translates by 
	the amount defined by the argument, aPoint."
	^ self copy moveBy: aPoint
]

{ #category : #accessing }
RubCharacterBlock >> width [
	^ surface width
]

{ #category : #accessing }
RubCharacterBlock >> withLeft: anInteger [
	^ self copy surface: (surface withLeft: anInteger)

]

{ #category : #accessing }
RubCharacterBlock >> withRight: anInteger [
	^ self copy surface: (surface withRight: anInteger)
]
