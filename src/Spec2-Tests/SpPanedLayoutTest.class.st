Class {
	#name : #SpPanedLayoutTest,
	#superclass : #SpLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #testing }
SpPanedLayoutTest class >> isAbstract [

	^ self == SpPanedLayoutTest
]

{ #category : #running }
SpPanedLayoutTest >> testElementsAreAddedInOrder [

	| second |
	layout addFirst: SpButtonPresenter new.
	layout addSecond: (second := SpButtonPresenter new).
	self assert: layout children last equals: second
]

{ #category : #running }
SpPanedLayoutTest >> testElementsAreAddedInOrderIndependentlyOfTheConfigurationOrder [

	| second |
	layout addSecond: (second := SpButtonPresenter new).
	layout addFirst: SpButtonPresenter new.
	self assert: layout children last equals: second
]

{ #category : #running }
SpPanedLayoutTest >> testLayoutWithOneFirstElementIsNotEmpty [

	layout addFirst: SpButtonPresenter new.
	self deny: layout isEmpty
]

{ #category : #running }
SpPanedLayoutTest >> testLayoutWithOneSecondElementIsNotEmpty [

	layout addSecond: SpButtonPresenter new.
	self deny: layout isEmpty
]

{ #category : #running }
SpPanedLayoutTest >> testRemoveFirstElementFromLayoutTakesItOut [

	| element |
	layout addFirst: (element := SpButtonPresenter new).
	layout remove: element.
	self assert: layout isEmpty
]

{ #category : #running }
SpPanedLayoutTest >> testRemoveSecondElementFromLayoutTakesItOut [

	| element |
	layout addSecond: (element := SpButtonPresenter new).
	layout remove: element.
	self assert: layout isEmpty
]

{ #category : #running }
SpPanedLayoutTest >> testReplaceFirstElementKeepsSingleElement [

	| replacement |
	layout addFirst: SpButtonPresenter new.
	layout addFirst: (replacement := SpButtonPresenter new).
	self assert: layout children size equals: 1
]

{ #category : #running }
SpPanedLayoutTest >> testReplaceFirstElementReplacesIt [

	| replacement |
	layout addFirst: SpButtonPresenter new.
	layout addFirst: (replacement := SpButtonPresenter new).
	self assert: layout children first equals: replacement
]

{ #category : #running }
SpPanedLayoutTest >> testReplaceSecondElementKeepsSingleElement [

	| replacement |
	layout addSecond: SpButtonPresenter new.
	layout addSecond: (replacement := SpButtonPresenter new).
	self assert: layout children size equals: 1
]

{ #category : #running }
SpPanedLayoutTest >> testReplaceSecondElementReplacesIt [

	| replacement |
	layout addSecond: SpButtonPresenter new.
	layout addSecond: (replacement := SpButtonPresenter new).
	self assert: layout children first equals: replacement
]
