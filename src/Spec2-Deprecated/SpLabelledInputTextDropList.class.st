"
I am a LabelledContainer specialized to manage a InputTextDropList.
See LabelledContainer

self example

I provide the accessors inputTextDropList to be more intuitive than subwidget, and the shortcuts input and dropList.

I provide shortcuts for the most common methods to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledInputTextDropList,
	#superclass : #SpLabelledContainer,
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #specs }
SpLabelledInputTextDropList class >> defaultSpec [
	<spec: #default>
	^ super labelLeft 
]

{ #category : #deprecation }
SpLabelledInputTextDropList class >> example [
	<sampleInstance>
	| example |
	example := self abstractExample.
	example
		placeholder: 'ghost text';
		items: {'item 1' . 'item 2'};
		displayBlock: [ :item | item asString ];
		extent: 450 @ 50;
		openWithSpec.
	^ example
]

{ #category : #deprecation }
SpLabelledInputTextDropList class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> acceptOnCR: aBoolean [
	^ self input acceptOnCR: aBoolean
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> beEncrypted [
	^ self input beEncrypted
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> displayBlock: aBlock [
	^ self dropList displayBlock: aBlock
]

{ #category : #accessing }
SpLabelledInputTextDropList >> dropList [
	^  self inputTextDropList dropList
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> emptyList [
	^ self dropList emptyList
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> encrypted: aBoolean [
	^ self input encrypted: aBoolean
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> entryCompletion: anEntryCompletion [
	^ self input entryCompletion: anEntryCompletion
]

{ #category : #'as yet unclassified' }
SpLabelledInputTextDropList >> ghostText: aText [
	self deprecated: 'This API is too tight to Morph. Use #placeholder: instead.' transformWith: '`@receiver ghostText: `@statement' -> '`@receiver placeholder: `@statement'.
	self placeholder: aText
]

{ #category : #initialization }
SpLabelledInputTextDropList >> initializePresenters [
super initializePresenters.
	self content: SpInputTextDropList.
]

{ #category : #accessing }
SpLabelledInputTextDropList >> input [
	^ self inputTextDropList input
]

{ #category : #accessing }
SpLabelledInputTextDropList >> inputTextDropList [
	^ subwidget
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> items: aList [
^ 	self dropList items: aList
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> listSize [
	^ self dropList listSize
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> placeholder: aText [
	^ self input placeholder: aText
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> resetSelection [
	^ self dropList resetSelection
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> selectedItem [
	^ self dropList selectedItem
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> setIndex: anIndex [
^ 	self dropList setIndex: anIndex
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> text [
	^ self input text
]

{ #category : #'api-shortcuts' }
SpLabelledInputTextDropList >> text: aText [
	^ self input text: aText
]

{ #category : #'events-shortcuts' }
SpLabelledInputTextDropList >> whenSelectedItemChanged: aBlock [
	self dropList
		whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledInputTextDropList >> whenSelectionChanged: aBlock [
	self dropList
		whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledInputTextDropList >> whenSelectionIndexChanged: aBlock [
	self dropList
		whenSelectionIndexChanged: aBlock
]
