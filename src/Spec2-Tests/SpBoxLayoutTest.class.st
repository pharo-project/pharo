Class {
	#name : #SpBoxLayoutTest,
	#superclass : #SpLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #testing }
SpBoxLayoutTest class >> isAbstract [

	^ self == SpBoxLayoutTest
]

{ #category : #private }
SpBoxLayoutTest >> extentOf: aPresenter [

	^ aPresenter adapter widget bounds extent
]

{ #category : #private }
SpBoxLayoutTest >> heightOf: aPresenter [

	^ (self extentOf: aPresenter) y
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

{ #category : #running }
SpBoxLayoutTest >> testReplaceWith [
	| p1 toReplace p3 replacement |

	layout add: (p1 := SpLabelPresenter new).
	layout add: (toReplace := SpLabelPresenter new).
	layout add: (p3 := SpLabelPresenter new).
	
	self assert: layout children second equals: toReplace.
	
	replacement := SpButtonPresenter new.
	layout replace: toReplace with: replacement.
	
	self assert: layout children second equals: replacement.
	self assert: layout children size equals: 3.
	self assert: layout children equals: { p1. replacement. p3 }
]

{ #category : #private }
SpBoxLayoutTest >> widthOf: aPresenter [

	^ (self extentOf: aPresenter) x
]
