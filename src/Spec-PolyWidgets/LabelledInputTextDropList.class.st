"
I am a LabelledContainer specialized to manage a InputTextDropList.
See LabelledContainer

self example

I provide the accessors inputTextDropList to be more intuitive than subwidget, and the shortcuts input and dropList.

I provide shortcuts for the most common methods to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #LabelledInputTextDropList,
	#superclass : #LabelledContainer,
	#category : #'Spec-PolyWidgets-Labelled'
}

{ #category : #specs }
LabelledInputTextDropList class >> defaultSpec [
	<spec: #default>
	^ super labelLeft 
]

{ #category : #example }
LabelledInputTextDropList class >> example [
	<sampleInstance>
	| example |
	example := self abstractExample.
	example
		ghostText: 'ghost text';
		items: {'item 1' . 'item 2'};
		displayBlock: [ :item | item asString ];
		extent: 450 @ 50;
		openWithSpec.
	^ example
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> acceptOnCR: aBoolean [
	^ self input acceptOnCR: aBoolean
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> beEncrypted [
	^ self input beEncrypted
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> displayBlock: aBlock [
	^ self dropList displayBlock: aBlock
]

{ #category : #accessing }
LabelledInputTextDropList >> dropList [
	^  self inputTextDropList dropList
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> emptyList [
	^ self dropList emptyList
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> encrypted: aBoolean [
	^ self input encrypted: aBoolean
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> entryCompletion: anEntryCompletion [
	^ self input entryCompletion: anEntryCompletion
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> ghostText: aText [
	^ self input ghostText: aText
]

{ #category : #initialization }
LabelledInputTextDropList >> initializeWidgets [
super initializeWidgets.
	self content: InputTextDropList.
]

{ #category : #accessing }
LabelledInputTextDropList >> input [
	^ self inputTextDropList input
]

{ #category : #accessing }
LabelledInputTextDropList >> inputTextDropList [
	^ subwidget
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> items: aList [
^ 	self dropList items: aList
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> listSize [
	^ self dropList listSize
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> resetSelection [
	^ self dropList resetSelection
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> selectedItem [
	^ self dropList selectedItem
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> setIndex: anIndex [
^ 	self dropList setIndex: anIndex
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> text [
	^ self input text
]

{ #category : #'api-shortcuts' }
LabelledInputTextDropList >> text: aText [
	^ self input text: aText
]

{ #category : #'events-shortcuts' }
LabelledInputTextDropList >> whenSelectedItemChanged: aBlock [
	self dropList
		whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
LabelledInputTextDropList >> whenSelectionChanged: aBlock [
	self dropList
		whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
LabelledInputTextDropList >> whenSelectionIndexChanged: aBlock [
	self dropList
		whenSelectionIndexChanged: aBlock
]
