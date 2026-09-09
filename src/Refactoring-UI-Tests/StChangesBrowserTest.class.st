"
A ChangesBrowserTest is a test class for testing the behavior of ChangesBrowser
"
Class {
	#name : 'StChangesBrowserTest',
	#superclass : 'TestCase',
	#instVars : [
		'browser'
	],
	#category : 'Refactoring-UI-Tests-UI',
	#package : 'Refactoring-UI-Tests',
	#tag : 'UI'
}

{ #category : 'running' }
StChangesBrowserTest >> setUp [
	super setUp.
	browser := StChangesBrowserPresenter new
]

{ #category : 'tests' }
StChangesBrowserTest >> testBuildDiffFor [
	self assert: (browser buildDiffFor: nil) equals: ''
]

{ #category : 'tests' }
StChangesBrowserTest >> testChanges [
	| aCollection |
	aCollection := {ReChange new}.
	browser changes: aCollection.
	self assert: browser changes equals: aCollection
]

{ #category : 'tests' }
StChangesBrowserTest >> testInitializeWidgets [
	self assert: browser changesTree isNotNil.
	self assert: browser textArea isNotNil.
	self
		assertCollection: browser focusOrder
		equals:
			{browser changesTree.
			browser textArea} asOrderedCollection
]

{ #category : 'tests' }
StChangesBrowserTest >> testPickedChanges [
	self assertEmpty: browser pickedChanges
]
