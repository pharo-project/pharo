Class {
	#name : #SpOverlayLayoutTest,
	#superclass : #SpLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #running }
SpOverlayLayoutTest >> initializeTestedInstance [

	layout := SpOverlayLayout new.
	presenter layout: layout
]

{ #category : #running }
SpOverlayLayoutTest >> testLayoutWithOneElementIsNotEmpty [

	layout child: SpButtonPresenter new.
	self deny: layout isEmpty
]

{ #category : #running }
SpOverlayLayoutTest >> testLayoutWithOverlayWidgetIsNotEmpty [

	layout addOverlay: SpButtonPresenter new.
	self deny: layout isEmpty
]

{ #category : #running }
SpOverlayLayoutTest >> testRemoveElementFromLayoutTakesItOut [

	| element |
	layout child: (element := SpButtonPresenter new).
	layout remove: element.
	self assert: layout isEmpty
]

{ #category : #running }
SpOverlayLayoutTest >> testRemoveOverlayFromLayoutTakesItOut [

	| element |
	layout addOverlay: (element := SpButtonPresenter new).
	layout remove: element.
	self assert: layout isEmpty
]

{ #category : #running }
SpOverlayLayoutTest >> testReplaceElementKeepsSingleElement [

	| replacement |
	layout child: SpButtonPresenter new.
	layout child: (replacement := SpButtonPresenter new).
	self assert: layout children size equals: 1
]
