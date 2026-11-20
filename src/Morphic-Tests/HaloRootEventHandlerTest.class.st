"
I check when the halo handler leaves a special gesture to the morphs. Each test builds its own root, not the world, and opens no halo.
"
Class {
	#name : 'HaloRootEventHandlerTest',
	#superclass : 'TestCase',
	#instVars : [
		'root',
		'handler'
	],
	#category : 'Morphic-Tests-Event',
	#package : 'Morphic-Tests',
	#tag : 'Event'
}

{ #category : 'running' }
HaloRootEventHandlerTest >> setUp [

	super setUp.
	root := WorldMorph new.
	root extent: 100 @ 100.
	handler := HaloRootEventHandler new
]

{ #category : 'helpers' }
HaloRootEventHandlerTest >> specialGestureAt: aPoint [
	"The blue (middle) button is the special gesture"

	^ MouseButtonEvent new
		  setType: #mouseDown
		  position: aPoint
		  which: MouseButtonEvent blueButton
		  buttons: MouseButtonEvent blueButton
		  hand: HandMorph new
		  stamp: 0
]

{ #category : 'tests' }
HaloRootEventHandlerTest >> testAClickOnTheWorldRemovesTheHalo [

	| hand target halo click |
	hand := HandMorph new.
	root addHand: hand.
	target := Morph new.
	root addMorph: target.
	halo := HaloMorph new.
	halo setTarget: target.
	root addMorph: halo.
	hand obtainHalo: halo.
	click := MouseButtonEvent new
		         setType: #mouseDown
		         position: 90 @ 90
		         which: MouseButtonEvent redButton
		         buttons: MouseButtonEvent redButton
		         hand: hand
		         stamp: 0.

	self deny: (handler handleEvent: click in: root).
	self assert: hand halo isNil
]

{ #category : 'tests' }
HaloRootEventHandlerTest >> testADropRemovesTheHalo [

	| hand target halo drop |
	hand := HandMorph new.
	root addHand: hand.
	target := Morph new.
	root addMorph: target.
	halo := HaloMorph new.
	halo setTarget: target.
	root addMorph: halo.
	hand obtainHalo: halo.
	drop := DropEvent new setPosition: 50 @ 50 contents: Morph new hand: hand.

	self deny: (handler handleEvent: drop in: root).
	self assert: hand halo isNil
]

{ #category : 'tests' }
HaloRootEventHandlerTest >> testDeletingTheTargetRemovesItsHalo [

	| hand target halo |
	hand := HandMorph new.
	root addHand: hand.
	target := Morph new.
	root addMorph: target.
	halo := HaloMorph new.
	halo setTarget: target.
	root addMorph: halo.
	hand obtainHalo: halo.

	target delete.
	halo step.

	self assert: halo owner isNil.
	self assert: hand halo isNil
]

{ #category : 'tests' }
HaloRootEventHandlerTest >> testLeavesAGestureInAnEmbeddedWorldToThatWorld [

	| world |
	world := WorldMorph new.
	world bounds: (0 @ 0 extent: 100 @ 100).
	world addMorph: (Morph new
		 position: 10 @ 10;
		 yourself).
	root addMorph: world.

	self deny: (handler handleEvent: (self specialGestureAt: 20 @ 20) in: root)
]

{ #category : 'tests' }
HaloRootEventHandlerTest >> testLeavesAGestureOnNothingToTheMorphs [

	self deny: (handler handleEvent: (self specialGestureAt: 50 @ 50) in: root)
]

{ #category : 'tests' }
HaloRootEventHandlerTest >> testLeavesAGestureToAMorphHandlingTheMiddleButton [

	root addMorph: (MorphHandlingMiddleButton new
		 position: 10 @ 10;
		 yourself).

	self deny: (handler handleEvent: (self specialGestureAt: 20 @ 20) in: root)
]

{ #category : 'tests' }
HaloRootEventHandlerTest >> testLeavesOrdinaryClicksToTheMorphs [

	| click |
	root addMorph: (Morph new
		 position: 10 @ 10;
		 yourself).
	click := MouseButtonEvent new
		         setType: #mouseDown
		         position: 20 @ 20
		         which: MouseButtonEvent redButton
		         buttons: MouseButtonEvent redButton
		         hand: HandMorph new
		         stamp: 0.

	self deny: (handler handleEvent: click in: root)
]
