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

{ #category : 'private' }
HaloRootEventHandlerTest >> click: aHand at: aPoint stamp: aStamp [
	"Special gesture click through aHand. Shift makes a second gesture pass the halo outwards"

	| buttons |
	buttons := MouseEvent redButton | UserInputEvent altKeyMask | UserInputEvent shiftKeyMask.
	#( #mouseDown #mouseUp ) do: [ :type |
		aHand handleEvent: (MouseButtonEvent new
			 setType: type
			 position: aPoint
			 which: buttons
			 buttons: buttons
			 hand: aHand
			 stamp: aStamp) ]
]

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
HaloRootEventHandlerTest >> testASecondGestureMovesTheHaloToTheOuterMorph [

	| hand outer inner |
	root extent: 300 @ 300.
	root addRootEventHandler: handler.
	hand := HandMorph new.
	root addHand: hand.
	outer := Morph new
		         extent: 100 @ 100;
		         yourself.
	inner := Morph new
		         extent: 50 @ 50;
		         yourself.
	outer addMorph: inner.
	inner position: 10 @ 10.
	root addMorph: outer.

	self click: hand at: inner center stamp: 0.
	self assert: hand halo target identicalTo: inner.
	"Far enough in time not to be a double click"
	self click: hand at: inner center stamp: 10000.
	self assert: hand halo target identicalTo: outer.
	hand halo delete
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
