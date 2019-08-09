"
I am a LabelledContainer specialized to manage a ListPresenter.
See LabelledContainer

self example

I provide the accessor list to be more intuitive than subwidget.

I provide shortcuts for the most common methods of the list api to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledListPresenter,
	#superclass : #SpLabelledContainerPrestenter,
	#category : #'Spec2-PolyWidgets-Labelled'
}

{ #category : #'api-shortcuts' }
SpLabelledListPresenter >> beMultipleSelection [
	^ self list beMultipleSelection
]

{ #category : #'api-shortcuts' }
SpLabelledListPresenter >> displayBlock: aBlock [
	^self list displayBlock: aBlock
]

{ #category : #initialization }
SpLabelledListPresenter >> initializeWidgets [
super initializeWidgets.
	self content: SpListPresenter
]

{ #category : #'api-shortcuts' }
SpLabelledListPresenter >> items: aList [
	^ self list items: aList
]

{ #category : #accessing }
SpLabelledListPresenter >> list [
	^ self subwidget
]

{ #category : #'api-shortcuts' }
SpLabelledListPresenter >> listSize [
	^ self list listSize
]

{ #category : #'api-shortcuts' }
SpLabelledListPresenter >> selectAll [
	^ self list selectAll
]

{ #category : #'api-shortcuts' }
SpLabelledListPresenter >> selectedItem [
	^ self list selectedItem
]

{ #category : #'api-shortcuts' }
SpLabelledListPresenter >> selectedItems [
	^ self list selectedItems
]

{ #category : #'api-shortcuts' }
SpLabelledListPresenter >> sortingBlock: aBlock [
	^self list sortingBlock: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledListPresenter >> whenSelectedItemChanged: aBlock [
self list whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledListPresenter >> whenSelectionChanged: aBlock [
	self list whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledListPresenter >> whenSelectionIndexChanged: aBlock [
	self list whenSelectionIndexChanged: aBlock
]
