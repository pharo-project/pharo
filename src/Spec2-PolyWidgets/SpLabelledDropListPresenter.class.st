"
I am a LabelledContainer specialized to manage a DropListPresenter.
See LabelledContainer

self example

I provide the accessor dropList to be more intuitive than subwidget.

I provide shortcuts for the most common methods of the droplist api to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledDropListPresenter,
	#superclass : #SpLabelledContainerPrestenter,
	#category : #'Spec2-PolyWidgets-Labelled'
}

{ #category : #specs }
SpLabelledDropListPresenter class >> defaultSpec [
	^ self labelLeft
]

{ #category : #'api-shortcuts' }
SpLabelledDropListPresenter >> displayBlock: aBlock [
	^self dropList displayBlock: aBlock
]

{ #category : #accessing }
SpLabelledDropListPresenter >> dropList [
	^ self subwidget
]

{ #category : #'api-shortcuts' }
SpLabelledDropListPresenter >> emptyList [
	^self dropList emptyList
]

{ #category : #initialization }
SpLabelledDropListPresenter >> initializeWidgets [
super initializeWidgets.
	self content: SpDropListPresenter
]

{ #category : #'api-shortcuts' }
SpLabelledDropListPresenter >> items: aList [
	^self dropList items: aList
]

{ #category : #'api-shortcuts' }
SpLabelledDropListPresenter >> listSize [
	^self dropList listSize
]

{ #category : #'api-shortcuts' }
SpLabelledDropListPresenter >> resetSelection [
	^self dropList resetSelection
]

{ #category : #'api-shortcuts' }
SpLabelledDropListPresenter >> selectedItem [
	^self dropList selectedItem
]

{ #category : #'api-shortcuts' }
SpLabelledDropListPresenter >> setIndex: anIndex [
	^self dropList setIndex: anIndex
]

{ #category : #'events-shortcuts' }
SpLabelledDropListPresenter >> whenSelectedItemChanged: aBlock [
	self dropList whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledDropListPresenter >> whenSelectionChanged: aBlock [
	self dropList whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledDropListPresenter >> whenSelectionIndexChanged: aBlock [
	self dropList whenSelectionIndexChanged: aBlock
]
