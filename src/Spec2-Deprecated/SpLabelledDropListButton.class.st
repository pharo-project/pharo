"
I am a LabelledContainer specialized to manage a DropListButton.
See LabelledContainer

self example

I provide the accessors dropListButton to be more intuitive than subwidget, and the shortcuts button and dropList.

I provide shortcuts for the most common methods to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledDropListButton,
	#superclass : #SpLabelledContainer,
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #specs }
SpLabelledDropListButton class >> defaultSpec [
	<spec: #default>
	^ super labelLeft 
]

{ #category : #deprecation }
SpLabelledDropListButton class >> example [
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

{ #category : #deprecation }
SpLabelledDropListButton class >> exampleUse [
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

{ #category : #deprecation }
SpLabelledDropListButton class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #'api-shortcuts' }
SpLabelledDropListButton >> action: aBlock [
	^self button action: aBlock
]

{ #category : #accessing }
SpLabelledDropListButton >> button [
	^ self dropListButton button
]

{ #category : #'api-shortcuts' }
SpLabelledDropListButton >> displayBlock: aBlock [
	^ self dropList display: aBlock
]

{ #category : #accessing }
SpLabelledDropListButton >> dropList [
	^ self dropListButton dropList
]

{ #category : #accessing }
SpLabelledDropListButton >> dropListButton [
	^ subwidget
]

{ #category : #'api-shortcuts' }
SpLabelledDropListButton >> emptyList [
	^self dropList emptyList
]

{ #category : #'api-shortcuts' }
SpLabelledDropListButton >> icon: anIcon [
^	self button icon: anIcon
]

{ #category : #initialization }
SpLabelledDropListButton >> initializePresenters [
super initializePresenters.
	self content: SpDropListButton.
]

{ #category : #'api-shortcuts' }
SpLabelledDropListButton >> items: aList [
	^self dropList items: aList
]

{ #category : #'api-shortcuts' }
SpLabelledDropListButton >> listSize [
	^ self dropList listSize
]

{ #category : #'api-shortcuts' }
SpLabelledDropListButton >> resetSelection [
^	self dropList resetSelection
]

{ #category : #'api-shortcuts' }
SpLabelledDropListButton >> selectedItem [
	^ self dropList selectedItem
]

{ #category : #'api-shortcuts' }
SpLabelledDropListButton >> setIndex: anIndex [
	^ self dropList setIndex: anIndex
]

{ #category : #'events-shortcuts' }
SpLabelledDropListButton >> whenSelectedItemChanged: aBlock [
	self dropList
		whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledDropListButton >> whenSelectionChanged: aBlock [
	self dropList
		whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledDropListButton >> whenSelectionIndexChanged: aBlock [
	self dropList
		whenSelectionIndexChanged: aBlock
]
