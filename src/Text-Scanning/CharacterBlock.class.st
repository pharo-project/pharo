"
I describe the location of one character displayed on the screen. My instances are used to return the results of methods:
	Paragraph characterBlockAtPoint: aPoint and
	Paragraph characterBlockForIndex: stringIndex.
Any recomposition or movement of a Paragraph can make the information I store stale.

text (Text): The text where my character is from
stringIndex (Integer): The index of my character in the text, starting from 1
textLine (TextLine): The displayed line my character is on
origin (Point): The top-left corner of the area allocated for displaying my
		character's glyph, in pixels, counting right then down from the
		top-left corner of the text display area, and starting from 0@0
corner (Point): The bottom-right corner of the area allocated for displaying my
		character's glyph, in pixels, counting right then down from the
		top-left corner of the text display area, and starting from 0@0

"
Class {
	#name : #CharacterBlock,
	#superclass : #Rectangle,
	#instVars : [
		'stringIndex',
		'text',
		'textLine'
	],
	#pools : [
		'TextConstants'
	],
	#category : #'Text-Scanning-Base'
}

{ #category : #comparing }
CharacterBlock >> < aCharacterBlock [ 
	"Answer whether the string index of the receiver precedes that of 
	aCharacterBlock."

	^stringIndex < aCharacterBlock stringIndex
]

{ #category : #comparing }
CharacterBlock >> <= aCharacterBlock [ 
	"Answer whether the string index of the receiver does not come after that 
	of aCharacterBlock."

	^(self > aCharacterBlock) not
]

{ #category : #comparing }
CharacterBlock >> = aCharacterBlock [

	^self species = aCharacterBlock species
		ifTrue: [ stringIndex = aCharacterBlock stringIndex]
		ifFalse: [ false]
]

{ #category : #comparing }
CharacterBlock >> > aCharacterBlock [ 
	"Answer whether the string index of the receiver comes after that of 
	aCharacterBlock."

	^aCharacterBlock < self
]

{ #category : #comparing }
CharacterBlock >> >= aCharacterBlock [ 
	"Answer whether the string index of the receiver does not precede that of 
	aCharacterBlock."

	^(self < aCharacterBlock) not
]

{ #category : #comparing }
CharacterBlock >> max: aCharacterBlock [
	aCharacterBlock ifNil:[^self].
	^aCharacterBlock > self
		ifTrue:[ aCharacterBlock]
		ifFalse:[self].
]

{ #category : #comparing }
CharacterBlock >> min: aCharacterBlock [
	aCharacterBlock ifNil:[^self].
	^aCharacterBlock < self
		ifTrue:[ aCharacterBlock]
		ifFalse:[self].
]

{ #category : #private }
CharacterBlock >> moveBy: aPoint [ 
	"Change the corner positions of the receiver so that its area translates by 
	the amount defined by the argument, aPoint."
	origin := origin + aPoint.
	corner := corner + aPoint
]

{ #category : #printing }
CharacterBlock >> printOn: aStream [

	aStream nextPutAll: 'a CharacterBlock with index '.
	stringIndex printOn: aStream.
	(text notNil and: [text notEmpty and: [ stringIndex between: 1 and: text size ]])
		ifTrue: [aStream nextPutAll: ' and character '.
				(text at: stringIndex) printOn: aStream].
	aStream nextPutAll: ' and rectangle '.
	super printOn: aStream.
	textLine ifNotNil: [aStream cr; nextPutAll: ' in '.
				textLine printOn: aStream].

]

{ #category : #accessing }
CharacterBlock >> stringIndex [
	"Answer the position of the receiver in the string it indexes."

	^stringIndex
]

{ #category : #private }
CharacterBlock >> stringIndex: anInteger text: aText topLeft: topLeft extent: extent [ 
	stringIndex := anInteger.
	text := aText.
	super 
		setPoint: topLeft
		point: topLeft + extent
]

{ #category : #accessing }
CharacterBlock >> textLine [
	^ textLine
]

{ #category : #accessing }
CharacterBlock >> textLine: aLine [ 
	textLine := aLine
]
