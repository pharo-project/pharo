"
A button in a morphic based tree
"
Class {
	#name : #MorphTreeColumnButton,
	#superclass : #SimpleButtonMorph,
	#instVars : [
		'model',
		'icon'
	],
	#category : #'Morphic-Widgets-Tree'
}

{ #category : #accessing }
MorphTreeColumnButton >> acceptDroppingMorph: aMorph event: evt [
	self model container swapColumn: self model withColumn: aMorph passenger.
	evt hand releaseMouseFocus: self.
	self noMorePotentialDropTarget.
	self model container changed.
	Cursor normal show.

]

{ #category : #accessing }
MorphTreeColumnButton >> asPotentialDropTarget [ 
	self model asPotentialDropTarget
]

{ #category : #'dropping/grabbing' }
MorphTreeColumnButton >> handlesMouseOverDragging: evt [
	^ true
]

{ #category : #accessing }
MorphTreeColumnButton >> icon [

	^ icon
]

{ #category : #accessing }
MorphTreeColumnButton >> icon: anIcon [
	
	icon ifNotNil: [ self removeMorph: icon ].
	
	anIcon ifNotNil: [
		icon := anIcon.
		self addMorphFront: anIcon ]
]

{ #category : #initialization }
MorphTreeColumnButton >> initialize [
	super initialize.
	self enableDragNDrop: true.
	self fillStyle: (self theme morphTreeColumnFillStyleFor: self).
	self borderStyle: (self theme morphTreeColumnBorderStyleFor: self)

]

{ #category : #testing }
MorphTreeColumnButton >> isDefault [
	^true
]

{ #category : #initialization }
MorphTreeColumnButton >> model [
	^ model
]

{ #category : #initialization }
MorphTreeColumnButton >> model: anObject [
	model := anObject
]

{ #category : #'event handling' }
MorphTreeColumnButton >> mouseDown: anEvent [
	| selectors |
	selectors := Array 
		with: #click:
		with: nil
		with: nil
		with: (self dragEnabled ifTrue:[#startDrag:] ifFalse:[nil]).
	anEvent hand waitForClicksOrDrag: self event: anEvent selectors: selectors threshold: 10.
	super mouseDown: anEvent
]

{ #category : #'dropping/grabbing' }
MorphTreeColumnButton >> mouseEnterDragging: evt [
	(evt hand hasSubmorphs and:[self dropEnabled]) ifFalse: ["no d&d"
		^ super mouseEnterDragging: evt].
	(self wantsDroppedMorph: evt hand firstSubmorph event: evt )
		ifTrue:[
			self asPotentialDropTarget.
		].
]

{ #category : #'dropping/grabbing' }
MorphTreeColumnButton >> mouseLeaveDragging: evt [
	self noMorePotentialDropTarget 
]

{ #category : #'dropping/grabbing' }
MorphTreeColumnButton >> mouseUp: evt [
	self noMorePotentialDropTarget.
	super mouseUp: evt
]

{ #category : #accessing }
MorphTreeColumnButton >> noMorePotentialDropTarget [
	self model noMorePotentialDropTarget
]

{ #category : #accessing }
MorphTreeColumnButton >> removeIcon [
	
	icon ifNotNil: [ self removeMorph: icon ].
]

{ #category : #'event handling' }
MorphTreeColumnButton >> startDrag: anEvent [
	| aTransferMorph |
	self dragEnabled ifFalse: [^ self].
	(anEvent hand hasSubmorphs)
		ifTrue: [^ self].
	oldColor 
		ifNotNil: [
			self fillStyle: oldColor.
			oldColor := nil].
	aTransferMorph := self model transferFor: self model from: self.
	aTransferMorph align: aTransferMorph draggedMorph center with: anEvent position.
	anEvent hand grabMorph: aTransferMorph.
	anEvent hand releaseMouseFocus: self
]

{ #category : #theme }
MorphTreeColumnButton >> themedFillStyle [
	^ self theme morphTreeColumnFillStyleFor: self
]

{ #category : #accessing }
MorphTreeColumnButton >> wantsDroppedMorph: aMorph event: anEvent [ 
	^ model container columnDropUnabled and: [(aMorph isTransferable) and: [(aMorph passenger isKindOf: MorphTreeColumn) and: [aMorph passenger ~= self model]]]
]
