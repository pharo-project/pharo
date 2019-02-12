"
SUnit tests for SpecFocusOrder
"
Class {
	#name : #SpecFocusOrderTest,
	#superclass : #TestCase,
	#instVars : [
		'specFocusOrder',
		'button1',
		'button2'
	],
	#category : #'Spec-Tests-Core-Support'
}

{ #category : #running }
SpecFocusOrderTest >> setUp [
	super setUp.
	specFocusOrder := SpecFocusOrder new.
	button1 := ButtonPresenter new.
	button2 := ButtonPresenter new
]

{ #category : #tests }
SpecFocusOrderTest >> testAdd [
	self
		assertEmpty: specFocusOrder presenters;
		shouldnt: [ specFocusOrder
				add: button1;
				add: button2;
				add: button1;
				add: button1 ]
			raise: Error;
		assert: specFocusOrder presenters equals: {button1 . button2 . button1 . button1} asOrderedCollection
]

{ #category : #tests }
SpecFocusOrderTest >> testAddLast [
	self
		assertEmpty: specFocusOrder presenters;
		shouldnt: [ specFocusOrder
				addLast: button1;
				addLast: button2 ]
			raise: Error;
		assert: specFocusOrder presenters equals: {button1 . button2} asOrderedCollection
]

{ #category : #tests }
SpecFocusOrderTest >> testGiveFocusToNextFrom [
	self
		assertEmpty: specFocusOrder presenters;
		shouldnt: [ specFocusOrder
				add: button1;
				add: button2 ]
			raise: Error;
		assert: (specFocusOrder giveFocusToNextFrom: button1 for: nil)
]

{ #category : #tests }
SpecFocusOrderTest >> testRemoveAll [
	self
		assertEmpty: specFocusOrder presenters;
		shouldnt: [ specFocusOrder
				add: button1;
				add: button2 ]
			raise: Error;
		assert: specFocusOrder presenters size == 2;
		shouldnt: [ specFocusOrder removeAll ] raise: Error;
		assertEmpty: specFocusOrder presenters
]
