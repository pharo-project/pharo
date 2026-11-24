"
I check which events the desktop handler leaves to the world's morphs, on a world of my own.
"
Class {
	#name : 'DesktopRootEventHandlerTest',
	#superclass : 'TestCase',
	#instVars : [
		'world',
		'hand',
		'handler'
	],
	#category : 'Morphic-Desktop-Tests',
	#package : 'Morphic-Desktop-Tests'
}

{ #category : 'helpers' }
DesktopRootEventHandlerTest >> escape [

	^ KeyboardEvent new
		  setType: #keystroke
		  buttons: 0
		  position: 0 @ 0
		  keyValue: 27
		  charCode: 27
		  hand: hand
		  stamp: 0
]

{ #category : 'helpers' }
DesktopRootEventHandlerTest >> mouseDownAt: aPoint [

	^ MouseButtonEvent new
		  setType: #mouseDown
		  position: aPoint
		  which: MouseButtonEvent redButton
		  buttons: MouseButtonEvent redButton
		  hand: hand
		  stamp: 0
]

{ #category : 'running' }
DesktopRootEventHandlerTest >> setUp [

	super setUp.
	world := WorldMorph new.
	hand := HandMorph new.
	world addHand: hand.
	handler := DesktopRootEventHandler new
]

{ #category : 'tests' }
DesktopRootEventHandlerTest >> testEscapeWithAKeyboardFocusIsLeftToTheFocus [

	| morph |
	morph := Morph new.
	world addMorph: morph.
	hand newKeyboardFocus: morph.

	self deny: (handler handleEvent: self escape in: world)
]

{ #category : 'tests' }
DesktopRootEventHandlerTest >> testMouseDownOnAMorphIsLeftToTheMorph [

	world addMorph: (Morph new
			 bounds: (0 @ 0 extent: 50 @ 50);
			 yourself).

	self deny: (handler handleEvent: (self mouseDownAt: 10 @ 10) in: world)
]

{ #category : 'tests' }
DesktopRootEventHandlerTest >> testOtherWindowEventsAreLeftToTheWorld [

	self deny: (handler handleEvent: (WindowEvent new
				  action: 1;
				  yourself) in: world)
]
