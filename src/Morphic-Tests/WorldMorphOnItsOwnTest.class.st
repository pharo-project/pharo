"
I check that a world works when created programmatically, without being the installed World: on its own, or embedded in another morph. An embedded world is the root of what it holds: its own root event handlers get the events meant for its morphs.
"
Class {
	#name : 'WorldMorphOnItsOwnTest',
	#superclass : 'TestCase',
	#instVars : [
		'outer',
		'inner',
		'outerHandler',
		'innerHandler'
	],
	#category : 'Morphic-Tests-Kernel',
	#package : 'Morphic-Tests',
	#tag : 'Kernel'
}

{ #category : 'helpers' }
WorldMorphOnItsOwnTest >> mouseDownAt: aPoint [

	^ MouseButtonEvent new
		  setType: #mouseDown
		  position: aPoint
		  which: MouseButtonEvent redButton
		  buttons: MouseButtonEvent redButton
		  hand: HandMorph new
		  stamp: 0
]

{ #category : 'running' }
WorldMorphOnItsOwnTest >> setUp [
	"An outer world holding a container, which embeds an inner world. Each world has a handler stub"

	| container |
	super setUp.
	outerHandler := MorphRootEventHandlerStub new.
	innerHandler := MorphRootEventHandlerStub new.
	outer := WorldMorph new.
	outer bounds: (0 @ 0 extent: 200 @ 200).
	outer addRootEventHandler: outerHandler.
	inner := WorldMorph new.
	inner bounds: (50 @ 50 extent: 100 @ 100).
	inner addRootEventHandler: innerHandler.
	container := Morph new.
	container bounds: (40 @ 40 extent: 120 @ 120).
	container addMorph: inner.
	outer addMorph: container
]

{ #category : 'tests' }
WorldMorphOnItsOwnTest >> testDrawsItsMorphsOnAForm [

	| world box form |
	world := WorldMorph new.
	world extent: 100 @ 100.
	box := Morph new
		       color: Color red;
		       bounds: (10 @ 10 extent: 20 @ 20);
		       yourself.
	world addMorph: box.

	form := world imageForm.

	self assert: (form colorAt: 20 @ 20) equals: Color red
]

{ #category : 'tests' }
WorldMorphOnItsOwnTest >> testHasNoRootEventHandlers [

	self assertEmpty: WorldMorph new rootEventHandlers
]

{ #category : 'tests' }
WorldMorphOnItsOwnTest >> testKeyDownsToAFocusInAnEmbeddedWorldGoToItsHandlers [

	| hand child |
	hand := HandMorph new.
	outer addHand: hand.
	child := Morph new.
	inner addMorph: child.
	hand newKeyboardFocus: child.

	hand handleEvent: ((KeyboardEvent new
		  setType: #keyDown
		  buttons: 0
		  position: 0 @ 0
		  keyValue: $a asInteger
		  charCode: $a asInteger
		  hand: hand
		  stamp: 0)
			 setHand: hand;
			 yourself).

	self assertEmpty: outerHandler handled.
	self assert: innerHandler handled asArray equals: { (#keyDown -> inner) }
]

{ #category : 'tests' }
WorldMorphOnItsOwnTest >> testMouseOutsideAnEmbeddedWorldGoesToTheOuterHandlers [

	outer processEvent: (self mouseDownAt: 10 @ 10).

	self assert: outerHandler handled asArray equals: { (#mouseDown -> outer) }.
	self assertEmpty: innerHandler handled
]

{ #category : 'tests' }
WorldMorphOnItsOwnTest >> testMouseOverAnEmbeddedWorldGoesToItsHandlers [

	outer processEvent: (self mouseDownAt: 100 @ 100).

	self assertEmpty: outerHandler handled.
	self assert: innerHandler handled asArray equals: { (#mouseDown -> inner) }
]
