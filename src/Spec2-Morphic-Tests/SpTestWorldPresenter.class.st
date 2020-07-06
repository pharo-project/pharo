"
I specialized SpWorldPresenter fo test purporses.
I hold an instance of World that you can access using #currentWorld.
I allows to test a WorldPresenter in a World that is not the current world.
"
Class {
	#name : #SpTestWorldPresenter,
	#superclass : #SpWorldPresenter,
	#instVars : [
		'worldMorph'
	],
	#classVars : [
		'TestWorld'
	],
	#category : #'Spec2-Morphic-Tests-Core-Support'
}

{ #category : #'ui requests' }
SpTestWorldPresenter class >> currentWorld [
	^ TestWorld ifNil: [ TestWorld := self newWorld ]
]

{ #category : #'instance creation' }
SpTestWorldPresenter class >> newWorld [
	| hand |
	hand := HandMorph new.
	^ WorldMorph new 
		addHand: hand;
		activeHand: hand;
		yourself
]

{ #category : #'ui requests' }
SpTestWorldPresenter class >> reset [
	TestWorld := nil
]

{ #category : #accessing }
SpTestWorldPresenter >> currentWorld [
	^ self class currentWorld
]

{ #category : #'private showing' }
SpTestWorldPresenter >> openWithSpecLayout: aLayout [
	super openWithSpecLayout: aLayout.
	isClosed := false
]

{ #category : #accessing }
SpTestWorldPresenter >> widget [
	^ self currentWorld
]
