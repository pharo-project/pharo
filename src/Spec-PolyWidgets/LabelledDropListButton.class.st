"
I am a LabelledContainer specialized to manage a DropListButton.
See LabelledContainer

self example

I provide the accessors dropListButton to be more intuitive than subwidget, and the shortcuts button and dropList.

I provide shortcuts for the most common methods to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #LabelledDropListButton,
	#superclass : #LabelledContainer,
	#category : #'Spec-PolyWidgets-Labelled'
}

{ #category : #specs }
LabelledDropListButton class >> defaultSpec [
	<spec: #default>
	^ super labelLeft 
]

{ #category : #example }
LabelledDropListButton class >> example [
	<sampleInstance>
	| example |
	example := self abstractExample.
	example button label: 'I am a button'.
	example
		items: {'item 1' . 'item 2'};
		displayBlock: [ :item | item asString ];
		extent: 550 @ 50;
		openWithSpec.
	^ example
]

{ #category : #example }
LabelledDropListButton class >> exampleUse [
	<sampleInstance>
	| example |
	example := self new.
	example button label: 'Add'.
	example label: 'Compose your team';
		displayBlock: [ :item | item asString ];
		items: {'Swordian' . 'Gardian' . 'Wizard' . 'Sniper'};
		extent: 300 @ 120;
		title: 'LabelDropButton example of use';
		openWithSpec: #labelTop.
	^ example
]

{ #category : #'api-shortcuts' }
LabelledDropListButton >> action: aBlock [
	^self button action: aBlock
]

{ #category : #accessing }
LabelledDropListButton >> button [
	^ self dropListButton button
]

{ #category : #'api-shortcuts' }
LabelledDropListButton >> displayBlock: aBlock [
	^self dropList displayBlock: aBlock
]

{ #category : #accessing }
LabelledDropListButton >> dropList [
	^ self dropListButton dropList
]

{ #category : #accessing }
LabelledDropListButton >> dropListButton [
	^ subwidget
]

{ #category : #'api-shortcuts' }
LabelledDropListButton >> emptyList [
	^self dropList emptyList
]

{ #category : #'api-shortcuts' }
LabelledDropListButton >> icon: anIcon [
^	self button icon: anIcon
]

{ #category : #initialization }
LabelledDropListButton >> initializeWidgets [
super initializeWidgets.
	self content: DropListButton.
]

{ #category : #'api-shortcuts' }
LabelledDropListButton >> items: aList [
	^self dropList items: aList
]

{ #category : #'api-shortcuts' }
LabelledDropListButton >> listSize [
	^ self dropList listSize
]

{ #category : #'api-shortcuts' }
LabelledDropListButton >> resetSelection [
^	self dropList resetSelection
]

{ #category : #'api-shortcuts' }
LabelledDropListButton >> selectedItem [
	^ self dropList selectedItem
]

{ #category : #'api-shortcuts' }
LabelledDropListButton >> setIndex: anIndex [
	^ self dropList setIndex: anIndex
]

{ #category : #'events-shortcuts' }
LabelledDropListButton >> whenSelectedItemChanged: aBlock [
	self dropList
		whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
LabelledDropListButton >> whenSelectionChanged: aBlock [
	self dropList
		whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
LabelledDropListButton >> whenSelectionIndexChanged: aBlock [
	self dropList
		whenSelectionIndexChanged: aBlock
]
