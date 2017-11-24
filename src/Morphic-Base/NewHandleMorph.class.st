Class {
	#name : #NewHandleMorph,
	#superclass : #HandleMorph,
	#instVars : [
		'hand',
		'waitingForClickInside'
	],
	#category : #'Morphic-Base-Widgets'
}

{ #category : #'submorphs-add/remove' }
NewHandleMorph >> delete [
	hand ifNotNil:[
		hand showTemporaryCursor: nil.
	].
	super delete.
]

{ #category : #all }
NewHandleMorph >> followHand: aHand forEachPointDo: block1 lastPointDo: block2 [
	hand := aHand.
	pointBlock := block1.
	lastPointBlock := block2.
	self position: hand lastEvent cursorPoint - (self extent // 2)
]

{ #category : #all }
NewHandleMorph >> followHand: aHand forEachPointDo: block1 lastPointDo: block2 withCursor: aCursor [
	hand := aHand.
	hand showTemporaryCursor: aCursor "hotSpotOffset: aCursor offset negated".
	borderWidth := 0.
	color := Color transparent.
	pointBlock := block1.
	lastPointBlock := block2.
	self position: hand lastEvent cursorPoint - (self extent // 2)
]

{ #category : #initialization }
NewHandleMorph >> initialize [
	"initialize the state of the receiver"

	super initialize.
	waitingForClickInside := true.
	
]

{ #category : #'dropping/grabbing' }
NewHandleMorph >> justDroppedInto: aMorph event: anEvent [
	"No dropping behavior because stepping will delete me.
	Moreover it needs to be done that way to evaluate lastPointBlock"

]

{ #category : #'wiw support' }
NewHandleMorph >> morphicLayerNumber [

	^1		"handles are very front-like - e.g. the spawn reframe logic actually asks if the first submorph of the world is one of us before deciding to create one"
]

{ #category : #'stepping and presenter' }
NewHandleMorph >> step [

	| eventSource |
	eventSource := hand lastEvent.
	eventSource anyButtonPressed
		ifTrue: [waitingForClickInside := false.
				self position: eventSource cursorPoint - (self extent // 2).
				pointBlock value: self center]
		ifFalse: [waitingForClickInside
					ifTrue: [(self containsPoint: eventSource cursorPoint)
								ifFalse: ["mouse wandered out before clicked"
										^ self delete]]
					ifFalse: [lastPointBlock value: self center.
							^ self delete]]
]
