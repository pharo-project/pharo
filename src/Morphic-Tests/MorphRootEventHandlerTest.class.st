"
I check that meta gestures go to the handler of the root morph.
"
Class {
	#name : 'MorphRootEventHandlerTest',
	#superclass : 'TestCase',
	#instVars : [
		'root',
		'child',
		'handler'
	],
	#category : 'Morphic-Tests-Events',
	#package : 'Morphic-Tests',
	#tag : 'Events'
}

{ #category : 'helpers' }
MorphRootEventHandlerTest >> keyDown: aCharacter [

	^ KeyboardEvent new
		  setType: #keyDown
		  buttons: 0
		  position: 0 @ 0
		  keyValue: aCharacter asInteger
		  charCode: aCharacter asInteger
		  hand: nil
		  stamp: 0
]

{ #category : 'helpers' }
MorphRootEventHandlerTest >> mouseDown [

	^ MouseButtonEvent new
		  setType: #mouseDown
		  position: 0 @ 0
		  which: MouseButtonEvent redButton
		  buttons: MouseButtonEvent redButton
		  hand: HandMorph new
		  stamp: 0
]

{ #category : 'running' }
MorphRootEventHandlerTest >> setUp [

	super setUp.
	root := WorldMorph new.
	root extent: 100 @ 100.
	child := Morph new.
	root addMorph: child.
	handler := MorphRootEventHandlerStub new
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testEventDeclinedByTheHandlersGoesDown [

	| received |
	received := false.
	child on: #mouseDown send: #value to: [ received := true ].
	root addRootEventHandler: (handler
		 decline;
		 yourself).

	root processEvent: self mouseDown.

	self assert: handler handled asArray equals: { (#mouseDown -> root) }.
	self assert: received
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testEventTakenByAHandlerDoesNotGoDown [

	| received event |
	received := false.
	child on: #mouseDown send: #value to: [ received := true ].
	root addRootEventHandler: handler.

	root processEvent: (event := self mouseDown).

	self assert: event wasHandled.
	self deny: received
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testHandlersAreAskedInOrderUntilOneTakesTheEvent [

	| declining last |
	declining := MorphRootEventHandlerStub new
		             decline;
		             yourself.
	last := MorphRootEventHandlerStub new.
	root
		addRootEventHandler: declining;
		addRootEventHandler: handler;
		addRootEventHandler: last.

	root processEvent: self mouseDown.

	self assert: declining handled asArray equals: { (#mouseDown -> root) }.
	self assert: handler handled asArray equals: { (#mouseDown -> root) }.
	self assertEmpty: last handled
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testKeyDownsFromTheHandGoToTheHandlersBeforeTheFocus [

	| hand received |
	hand := HandMorph new.
	root addHand: hand.
	received := false.
	child on: #keyDown send: #value to: [ received := true ].
	hand newKeyboardFocus: child.
	root addRootEventHandler: handler.

	hand handleEvent: ((self keyDown: $a)
		 setHand: hand;
		 yourself).

	self assert: handler handled asArray equals: { (#keyDown -> root) }.
	self deny: received
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testNavigationKeyGoesToTheRootHandler [

	root addRootEventHandler: handler.

	self assert: (root navigationKey: (self keyDown: $a)).
	self assert: handler handled asArray equals: { (#navigationKey -> root) }
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testNavigationKeyWithoutHandlerIsIgnored [

	self deny: (root navigationKey: (self keyDown: $a))
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testRemovedHandlerIsNoLongerAsked [

	root addRootEventHandler: handler.
	root removeRootEventHandler: handler.

	root processEvent: self mouseDown.

	self assertEmpty: handler handled
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testRootWithoutHandlerLetsEventsGoDown [

	| received |
	received := false.
	child on: #mouseDown send: #value to: [ received := true ].

	root processEvent: self mouseDown.

	self assertEmpty: root rootEventHandlers.
	self assert: received
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testSubmorphsDoNotAskTheHandlers [

	root addRootEventHandler: handler.

	child processEvent: self mouseDown.

	self assertEmpty: handler handled
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testWindowCloseGoesToTheRootHandler [

	| close |
	root addRootEventHandler: handler.
	close := WindowEvent new
		         action: 2;
		         yourself. "#windowClose"

	root windowEvent: close.

	self assert: handler handled asArray equals: { (#windowClose -> root) }
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testWorldMenusAndDragsGoToTheRootHandler [

	root addRootEventHandler: handler.

	root
		invokeWorldMenu: self mouseDown;
		popUpContentsMenu: self mouseDown;
		dragThroughOnDesktop: self mouseDown.

	self assert: handler handled asArray equals: {
			(#worldMenu -> root).
			(#contentsMenu -> root).
			(#dragThrough -> root) }
]
