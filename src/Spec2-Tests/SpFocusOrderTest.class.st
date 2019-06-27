"
SUnit tests for SpecFocusOrder
"
Class {
	#name : #SpFocusOrderTest,
	#superclass : #TestCase,
	#instVars : [
		'specFocusOrder',
		'button1',
		'button2'
	],
	#category : #'Spec2-Tests-Core-Support'
}

{ #category : #running }
SpFocusOrderTest >> setUp [
	super setUp.
	specFocusOrder := SpFocusOrder new.
	button1 := SpButtonPresenter new.
	button2 := SpButtonPresenter new
]

{ #category : #tests }
SpFocusOrderTest >> testAdd [
	self
		assert: specFocusOrder presenters isEmpty;
		shouldnt: [ specFocusOrder
				add: button1;
				add: button2;
				add: button1;
				add: button1 ]
			raise: Error;
		assert: specFocusOrder presenters
			equals:
			{button1.
			button2.
			button1.
			button1} asOrderedCollection
]

{ #category : #tests }
SpFocusOrderTest >> testAddLast [
	self
		assert: specFocusOrder presenters isEmpty;
		shouldnt: [ specFocusOrder
				addLast: button1;
				addLast: button2]
			raise: Error;
		assert: specFocusOrder presenters
			equals:
			{button1.
			button2} asOrderedCollection
]

{ #category : #tests }
SpFocusOrderTest >> testGiveFocusToNextFrom [
	self
		assert: specFocusOrder presenters isEmpty;
		shouldnt: [ specFocusOrder
				add: button1;
				add: button2 ]
			raise: Error;
			assert: (specFocusOrder giveFocusToNextFrom:button1 for: nil).
]

{ #category : #tests }
SpFocusOrderTest >> testRemoveAll [
	self
		assert: specFocusOrder presenters isEmpty;
		shouldnt: [ specFocusOrder
				add: button1;
				add: button2 ]
			raise: Error;
		assert: specFocusOrder presenters size == 2;
		shouldnt: [ specFocusOrder removeAll ] raise: Error;
		assert: specFocusOrder presenters isEmpty
]
