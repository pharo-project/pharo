"
I am a LabelledContainer specialized to manage a DropListPresenter.
See LabelledContainer

self example

I provide the accessor dropList to be more intuitive than subwidget.

I provide shortcuts for the most common methods of the droplist api to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledDropList,
	#superclass : #SpLabelledContainer,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpLabelledDropList class >> defaultSpec [
	^ self labelLeft
]

{ #category : #'api-shortcuts' }
SpLabelledDropList >> display: aBlock [

	^ self dropList display: aBlock
]

{ #category : #'api-shortcuts' }
SpLabelledDropList >> displayBlock: aBlock [

	self
		deprecated: 'Please use #display: instead'
		transformWith: '`@receiver displayBlock: `@arg' 
						-> '`@receiver display: `@arg'.

	^self dropList display: aBlock
]

{ #category : #accessing }
SpLabelledDropList >> dropList [
	^ self subwidget
]

{ #category : #'api-shortcuts' }
SpLabelledDropList >> emptyList [
	^self dropList emptyList
]

{ #category : #initialization }
SpLabelledDropList >> initializePresenters [
super initializePresenters.
	self content: SpDropListPresenter
]

{ #category : #'api-shortcuts' }
SpLabelledDropList >> items: aList [
	^self dropList items: aList
]

{ #category : #'api-shortcuts' }
SpLabelledDropList >> listSize [
	^self dropList listSize
]

{ #category : #'api-shortcuts' }
SpLabelledDropList >> resetSelection [
	^self dropList resetSelection
]

{ #category : #'api-shortcuts' }
SpLabelledDropList >> selectedItem [
	^self dropList selectedItem
]

{ #category : #'api-shortcuts' }
SpLabelledDropList >> setIndex: anIndex [
	^self dropList setIndex: anIndex
]

{ #category : #'events-shortcuts' }
SpLabelledDropList >> whenSelectedItemChanged: aBlock [
	self dropList whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledDropList >> whenSelectionChanged: aBlock [
	self dropList whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledDropList >> whenSelectionIndexChanged: aBlock [
	self dropList whenSelectionIndexChanged: aBlock
]
