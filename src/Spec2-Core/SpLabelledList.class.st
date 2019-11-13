"
I am a LabelledContainer specialized to manage a ListPresenter.
See LabelledContainer

self example

I provide the accessor list to be more intuitive than subwidget.

I provide shortcuts for the most common methods of the list api to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledList,
	#superclass : #SpLabelledContainer,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #'api-shortcuts' }
SpLabelledList >> beMultipleSelection [
	^ self list beMultipleSelection
]

{ #category : #'api-shortcuts' }
SpLabelledList >> display: aBlock [

	^ self list display: aBlock
]

{ #category : #'api-shortcuts' }
SpLabelledList >> displayBlock: aBlock [

	self
		deprecated: 'Please use #display: instead'
		transformWith: '`@receiver displayBlock: `@arg' 
						-> '`@receiver display: `@arg'.

	^ self list display: aBlock
]

{ #category : #initialization }
SpLabelledList >> initializePresenters [
super initializePresenters.
	self content: SpListPresenter
]

{ #category : #'api-shortcuts' }
SpLabelledList >> items: aList [
	^ self list items: aList
]

{ #category : #accessing }
SpLabelledList >> list [
	^ self subwidget
]

{ #category : #'api-shortcuts' }
SpLabelledList >> listSize [
	^ self list listSize
]

{ #category : #'api-shortcuts' }
SpLabelledList >> selectAll [
	^ self list selectAll
]

{ #category : #'api-shortcuts' }
SpLabelledList >> selectedItem [
	^ self list selectedItem
]

{ #category : #'api-shortcuts' }
SpLabelledList >> selectedItems [
	^ self list selectedItems
]

{ #category : #'api-shortcuts' }
SpLabelledList >> sortingBlock: aBlock [
	^self list sortingBlock: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledList >> whenSelectedItemChanged: aBlock [
self list whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledList >> whenSelectionChanged: aBlock [
	self list whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledList >> whenSelectionIndexChanged: aBlock [
	self list whenSelectionIndexChanged: aBlock
]
