Class {
	#name : 'HaloMorphTest',
	#superclass : 'TestCase',
	#instVars : [
		'outerMorph',
		'innerMorph'
	],
	#category : 'Morphic-Tests-Widgets',
	#package : 'Morphic-Tests',
	#tag : 'Widgets'
}

{ #category : 'running' }
HaloMorphTest >> setUp [

	super setUp.
	outerMorph := Morph new
		color: Color red;
		extent: 100@100;
		yourself.

	innerMorph := Morph new
		color: Color green;
		extent: 50@50;
		yourself.

	outerMorph addMorph: innerMorph.
	innerMorph position: 10@10
]

{ #category : 'tests' }
HaloMorphTest >> simulateSpecialGesture: aPoint [

	| buttons hand evt |
	buttons := MouseEvent redButton | UserInputEvent altKeyMask | UserInputEvent shiftKeyMask.
	hand := self currentWorld hands first.

	evt := (MouseButtonEvent new
		setType: #mouseDown
		position: aPoint
		which:  buttons
		buttons: buttons
		hand: hand
		stamp: Time millisecondClockValue).

	hand handleEvent: evt.

	evt := (MouseButtonEvent new
		setType: #mouseUp
		position: aPoint
		which:  buttons
		buttons: buttons
		hand: hand
		stamp: Time millisecondClockValue).

	hand handleEvent: evt
]

{ #category : 'running' }
HaloMorphTest >> tearDown [

	innerMorph ifNotNil: [ innerMorph removeHalo; delete ].
	outerMorph ifNotNil: [ outerMorph removeHalo; delete ].

	self currentHand initForEvents.

	super tearDown
]

{ #category : 'tests' }
HaloMorphTest >> testSpecialGestureOnInnerMorphPutsHaloInInnerMorph [

	outerMorph openInWorld.
	self simulateSpecialGesture: innerMorph center.

	self assert: self currentWorld hands first halo target equals: innerMorph
]

{ #category : 'tests' }
HaloMorphTest >> testSpecialGestureOnInnerMorphTwiceGoesToOuterMorph [

	outerMorph openInWorld.

	self simulateSpecialGesture: innerMorph center.
	"I have to wait to avoid being detected as double-click"
	HandMorph doubleClickTime milliSeconds wait.
	self simulateSpecialGesture: innerMorph center.

	self assert: self currentWorld hands first halo target equals: outerMorph
]

{ #category : 'tests' }
HaloMorphTest >> testSpecialGestureOnOuterMorphPutsHaloInOuterMorph [

	outerMorph openInWorld.
	self simulateSpecialGesture: outerMorph position + (1@1).

	self assert: self currentWorld hands first halo target equals: outerMorph
]
