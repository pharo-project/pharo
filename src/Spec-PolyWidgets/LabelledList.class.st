"
I am a LabelledContainer specialized to manage a ListPresenter.
See LabelledContainer

self example

I provide the accessor list to be more intuitive than subwidget.

I provide shortcuts for the most common methods of the list api to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #LabelledList,
	#superclass : #LabelledContainer,
	#category : #Spec-PolyWidgets-Labelled
}

{ #category : #example }
LabelledList class >> example [
	| example |
	example := self abstractExample.
	example
		items: {'item 1' . 'item 2'};
		openWithSpec.
	^ example
]

{ #category : #api-shortcuts }
LabelledList >> beMultipleSelection [
	^ self list beMultipleSelection
]

{ #category : #api-shortcuts }
LabelledList >> displayBlock: aBlock [
	^self list displayBlock: aBlock
]

{ #category : #api-shortcuts }
LabelledList >> filteringBlock: aBlock [
	^ self list filteringBlock: aBlock
]

{ #category : #initialization }
LabelledList >> initializeWidgets [
super initializeWidgets.
	self content: ListPresenter
]

{ #category : #api-shortcuts }
LabelledList >> items: aList [
	^ self list items: aList
]

{ #category : #accessing }
LabelledList >> list [
	^ self subwidget
]

{ #category : #api-shortcuts }
LabelledList >> listSize [
	^ self list listSize
]

{ #category : #api-shortcuts }
LabelledList >> selectAll [
	^ self list selectAll
]

{ #category : #api-shortcuts }
LabelledList >> selectedItem [
	^ self list selectedItem
]

{ #category : #api-shortcuts }
LabelledList >> selectedItems [
	^ self list selectedItems
]

{ #category : #api-shortcuts }
LabelledList >> sortingBlock: aBlock [
	^self list sortingBlock: aBlock
]

{ #category : #events-shortcuts }
LabelledList >> whenSelectedItemChanged: aBlock [
self list whenSelectedItemChanged: aBlock
]

{ #category : #events-shortcuts }
LabelledList >> whenSelectionChanged: aBlock [
	self list whenSelectionChanged: aBlock
]

{ #category : #events-shortcuts }
LabelledList >> whenSelectionIndexChanged: aBlock [
	self list whenSelectionIndexChanged: aBlock
]
