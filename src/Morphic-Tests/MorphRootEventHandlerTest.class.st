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
MorphRootEventHandlerTest >> testMetaMenuGestureGoesToTheRootHandler [

	root rootEventHandler: handler.

	child invokeMetaMenu: self mouseDown.

	self assert: handler handled asArray equals: { (#metaMenuGesture -> child) }
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testMorphsAskTheHandlerOfTheirRoot [

	root rootEventHandler: handler.

	self assert: child rootEventHandler identicalTo: handler.
	self assert: root rootEventHandler identicalTo: handler
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testRootWithoutHandlerIgnoresMetaGestures [

	self assert: child rootEventHandler class identicalTo: MorphRootEventHandler
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testSpecialGestureGoesToTheRootHandler [

	root rootEventHandler: handler.

	child handleSpecialGesture: self mouseDown.

	self assert: handler handled asArray equals: { (#specialGesture -> child) }
]

{ #category : 'tests' }
MorphRootEventHandlerTest >> testYellowButtonMenuGoesToTheRootHandler [

	root rootEventHandler: handler.
	root setProperty: #wantsYellowButtonMenu toValue: true.

	root yellowButtonActivity: false.

	self assert: handler handled asArray equals: { (#yellowButtonMenu -> root) }
]
