"
A ChangesBrowserTest is a test class for testing the behavior of ChangesBrowser
"
Class {
	#name : #ChangesBrowserTest,
	#superclass : #TestCase,
	#instVars : [
		'browser'
	],
	#category : #'NautilusRefactoring-Tests'
}

{ #category : #running }
ChangesBrowserTest >> setUp [
	super setUp.
	browser := ChangesBrowser new
]

{ #category : #tests }
ChangesBrowserTest >> testBuildDiffFor [
	self assert: (browser buildDiffFor: nil) equals: ''
]

{ #category : #tests }
ChangesBrowserTest >> testChanges [
	| aCollection |
	aCollection := {RBRefactoryChange new}.
	browser changes: aCollection.
	self assert: browser changes equals: aCollection
]

{ #category : #tests }
ChangesBrowserTest >> testInitializeWidgets [
	self assert: browser changesTree isNotNil.
	self assert: browser textArea isNotNil.
	self
		assertCollection: browser focusOrder
		equals:
			{browser changesTree.
			browser textArea} asOrderedCollection
]

{ #category : #tests }
ChangesBrowserTest >> testPickedChanges [
	self assert: browser pickedChanges isEmpty
]
