"
A HandleMorph provides mouse-up control behavior.
"
Class {
	#name : #HandleMorph,
	#superclass : #EllipseMorph,
	#instVars : [
		'pointBlock',
		'lastPointBlock'
	],
	#category : #'Morphic-Base-Widgets'
}

{ #category : #initialize }
HandleMorph >> forEachPointDo: aBlock [
	pointBlock := aBlock
]

{ #category : #initialize }
HandleMorph >> forEachPointDo: aBlock lastPointDo: otherBlock [
	pointBlock := aBlock.
	lastPointBlock := otherBlock
]

{ #category : #initialization }
HandleMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	""
	self extent: 8 @ 8.
	
]

{ #category : #'dropping/grabbing' }
HandleMorph >> justDroppedInto: aMorph event: anEvent [
	"So that when the hand drops me (into the world) I go away"
	self removeHalo.
	lastPointBlock ifNotNil: [lastPointBlock value: self center]. "Probably unnecessary"
	anEvent hand releaseKeyboardFocus: self.
	self changed.
	self delete.

]

{ #category : #'event handling' }
HandleMorph >> keyStroke: evt [
	"Check for cursor keys"

	| keyValue |
	owner isHandMorph ifFalse: [ ^ self ].
	keyValue := evt keyValue.
	keyValue = 28 ifTrue: [ ^ self position: self position - (1 @ 0) ].
	keyValue = 29 ifTrue: [ ^ self position: self position + (1 @ 0) ].
	keyValue = 30 ifTrue: [ ^ self position: self position - (0 @ 1) ].
	keyValue = 31 ifTrue: [ ^ self position: self position + (0 @ 1) ].
	"Special case for return"
	keyValue = 13
		ifTrue: [ "Drop the receiver and be done"
			"Probably unnecessary"
			owner releaseKeyboardFocus: self.
			self delete ]
]

{ #category : #stepping }
HandleMorph >> step [
	pointBlock value: self center
]

{ #category : #stepping }
HandleMorph >> stepTime [
	"Update every hundredth of a second."
	^ 10

]
