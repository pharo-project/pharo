"
An AthensWrapWorldMorph is a AthensWrapMorph holding a specialized Athens WorldMorph. This Athens WorldMorph draws the submorphs with an AthensCanvas.

Instance Variables
	worldmorph:		the Athens WorldMorph
	wrappedWorldDisplay:		a display wrapper

"
Class {
	#name : #AthensWrapWorldMorph,
	#superclass : #AthensWrapMorph,
	#instVars : [
		'worldmorph',
		'wrappedWorldDisplay'
	],
	#category : #'Athens-Morphic'
}

{ #category : #initialization }
AthensWrapWorldMorph >> defaultExtent [
	^ 400 @ 400
]

{ #category : #initialization }
AthensWrapWorldMorph >> initialize [
	super initialize.
	self extent: self defaultExtent.
	self hResizing: #spaceFill.
	self vResizing: #spaceFill.
	wrappedWorldDisplay := AthensWrappedWorldDisplay on: self.
	worldmorph := AthensWorldMorph new.
	worldmorph color: Color orange.
	worldmorph addHand: HandMorph new.
	worldmorph display: wrappedWorldDisplay.
	self addMorph: worldmorph
]

{ #category : #layout }
AthensWrapWorldMorph >> layoutChanged [
	super layoutChanged.
	worldmorph ifNotNil:[:w |  w extent: self extent]
]

{ #category : #stepping }
AthensWrapWorldMorph >> step [
	self changed
]

{ #category : #stepping }
AthensWrapWorldMorph >> stepTime [
	^ 50
]
