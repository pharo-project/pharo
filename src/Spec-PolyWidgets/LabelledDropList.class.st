"
I am a LabelledContainer specialized to manage a DropListPresenter.
See LabelledContainer

self example

I provide the accessor dropList to be more intuitive than subwidget.

I provide shortcuts for the most common methods of the droplist api to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #LabelledDropList,
	#superclass : #LabelledContainer,
	#category : #'Spec-PolyWidgets-Labelled'
}

{ #category : #specs }
LabelledDropList class >> defaultSpec [
	<spec: #default>
	^ super labelLeft

]

{ #category : #example }
LabelledDropList class >> example [
	<sampleInstance>
	| example |
	example := self abstractExample.
example
		items: {'item 1' . 'item 2'};
		displayBlock: [ :item | item asString ];
		extent: 400 @ 50;
		openWithSpec.
	^ example
]

{ #category : #'api-shortcuts' }
LabelledDropList >> displayBlock: aBlock [
	^self dropList displayBlock: aBlock
]

{ #category : #accessing }
LabelledDropList >> dropList [
	^ self subwidget
]

{ #category : #'api-shortcuts' }
LabelledDropList >> emptyList [
	^self dropList emptyList
]

{ #category : #initialization }
LabelledDropList >> initializeWidgets [
super initializeWidgets.
	self content: DropListPresenter
]

{ #category : #'api-shortcuts' }
LabelledDropList >> items: aList [
	^self dropList items: aList
]

{ #category : #'api-shortcuts' }
LabelledDropList >> listSize [
	^self dropList listSize
]

{ #category : #'api-shortcuts' }
LabelledDropList >> resetSelection [
	^self dropList resetSelection
]

{ #category : #'api-shortcuts' }
LabelledDropList >> selectedItem [
	^self dropList selectedItem
]

{ #category : #'api-shortcuts' }
LabelledDropList >> setIndex: anIndex [
	^self dropList setIndex: anIndex
]

{ #category : #'events-shortcuts' }
LabelledDropList >> whenSelectedItemChanged: aBlock [
	self dropList whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
LabelledDropList >> whenSelectionChanged: aBlock [
	self dropList whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
LabelledDropList >> whenSelectionIndexChanged: aBlock [
	self dropList whenSelectionIndexChanged: aBlock
]
