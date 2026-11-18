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
MorphRootEventHandlerTest >> mouseDown [

	^ MouseButtonEvent new
		  setType: #mouseDown
		  position: 0 @ 0
		  which: MouseButtonEvent redButton
		  buttons: MouseButtonEvent redButton
		  hand: nil
		  stamp: 0
]

{ #category : 'running' }
MorphRootEventHandlerTest >> setUp [

	super setUp.
	root := Morph new.
	child := Morph new.
	root addMorph: child.
	handler := MorphRootEventHandlerStub new
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testDeletingAMorphNotifiesEveryHandler [

	| other |
	other := MorphRootEventHandlerStub new.
	root
		addRootEventHandler: handler;
		addRootEventHandler: other.

	child delete.

	self assert: handler handled asArray equals: { #deleted -> child }.
	self assert: other handled asArray equals: { #deleted -> child }
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testHandlersAreAskedInOrderUntilOneHandlesTheGesture [

	| declining last |
	declining := MorphRootEventHandlerStub new decline; yourself.
	last := MorphRootEventHandlerStub new.
	root
		addRootEventHandler: declining;
		addRootEventHandler: handler;
		addRootEventHandler: last.

	self assert: (child handleSpecialGesture: self mouseDown).

	self assert: declining handled asArray equals: { #specialGesture -> child }.
	self assert: handler handled asArray equals: { #specialGesture -> child }.
	self assertEmpty: last handled
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testMetaMenuGestureGoesToTheRootHandler [

	root addRootEventHandler: handler.

	child invokeMetaMenu: self mouseDown.

	self assert: handler handled asArray equals: { #metaMenuGesture -> child }
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testMorphsAskTheHandlersOfTheirRoot [

	root addRootEventHandler: handler.

	self assert: child rootEventHandlers asArray equals: { handler }
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testRemovedHandlerIsNoLongerAsked [

	root addRootEventHandler: handler.
	root removeRootEventHandler: handler.

	child handleSpecialGesture: self mouseDown.

	self assertEmpty: handler handled
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testRootWithoutHandlerIgnoresMetaGestures [

	self assertEmpty: child rootEventHandlers.
	self deny: (child handleSpecialGesture: self mouseDown)
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testSpecialGestureGoesToTheRootHandler [

	root addRootEventHandler: handler.

	child handleSpecialGesture: self mouseDown.

	self assert: handler handled asArray equals: { #specialGesture -> child }
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testWrappingAMorphNotifiesTheHandlers [

	root addRootEventHandler: handler.

	child addFlexShell.

	self assert: handler handled asArray equals: { #wrapped -> child }
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testYellowButtonMenuGoesToTheRootHandler [

	root addRootEventHandler: handler.
	root setProperty: #wantsYellowButtonMenu toValue: true.

	root yellowButtonActivity: false.

	self assert: handler handled asArray equals: { #yellowButtonMenu -> root }
]
