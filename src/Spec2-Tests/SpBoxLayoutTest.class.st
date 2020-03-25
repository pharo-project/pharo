Class {
	#name : #SpBoxLayoutTest,
	#superclass : #SpLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #testing }
SpBoxLayoutTest class >> isAbstract [

	^ self == SpBoxLayoutTest
]

{ #category : #running }
SpBoxLayoutTest >> testElementsAreAddedInOrder [

	| second |
	layout add: SpButtonPresenter new.
	layout add: (second := SpButtonPresenter new).
	self assert: layout children last equals: second
]

{ #category : #running }
SpBoxLayoutTest >> testLayoutWithOneElementIsNotEmpty [

	layout add: SpButtonPresenter new.
	self deny: layout isEmpty
]

{ #category : #running }
SpBoxLayoutTest >> testRemoveElementFromLayoutTakesItOut [

	| element |
	layout add: (element := SpButtonPresenter new).
	layout remove: element.
	self assert: layout isEmpty
]
