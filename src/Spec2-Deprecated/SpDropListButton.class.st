"
I am an elementary Spec widget that associates a DropListPresenter and a ButtonPresenter.

self example

I provide accessors dropList and input and shortcuts for the most common methods of their api.
"
Class {
	#name : #SpDropListButton,
	#superclass : #SpPresenter,
	#instVars : [
		'dropList',
		'button'
	],
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #specs }
SpDropListButton class >> defaultSpec [
	<spec: #default>
	^ SpLayout composed
		newRow: [ :row | 
			row
				add: #dropList origin: 0 @ 0 corner: 0.8 @ 1;
				add: #button origin: 0.8 @ 0 corner: 1 @ 1 ];
		yourself
]

{ #category : #deprecation }
SpDropListButton class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		displayBlock: [ :item | item asString ];
		items: {'Swordian' . 'Gardian' . 'Wizard' . 'Sniper'};
		label: 'Add';
		extent: 300 @ 70;
		openWithSpec.
	example withWindowDo: [ :window | window title: 'DropListButton example' ].
	^ example
]

{ #category : #deprecation }
SpDropListButton class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #'api-shortcuts' }
SpDropListButton >> action: aBlock [
	self button action: aBlock
]

{ #category : #accessing }
SpDropListButton >> button [
	^ button
]

{ #category : #'api-shortcuts' }
SpDropListButton >> displayBlock: aBlock [
	self dropList display: aBlock
]

{ #category : #accessing }
SpDropListButton >> dropList [
	^ dropList
]

{ #category : #'api-shortcuts' }
SpDropListButton >> emptyList [
	self dropList emptyList
]

{ #category : #'api-shortcuts' }
SpDropListButton >> icon: anIcon [
	self button icon: anIcon
]

{ #category : #initialization }
SpDropListButton >> initializePresenters [
	dropList := self instantiate: SpDropListPresenter.
	button := self instantiate: SpButtonPresenter.
	self focusOrder
		add: dropList;
		add: button
]

{ #category : #'api-shortcuts' }
SpDropListButton >> items: aList [
	self dropList items: aList
]

{ #category : #'api-shortcuts' }
SpDropListButton >> label [
	self button label
]

{ #category : #'api-shortcuts' }
SpDropListButton >> label: aStringOrImageMorph [
	self button label: aStringOrImageMorph
]

{ #category : #'api-shortcuts' }
SpDropListButton >> listSize [
	self dropList listSize
]

{ #category : #'api-shortcuts' }
SpDropListButton >> resetSelection [
	self dropList resetSelection
]

{ #category : #'api-shortcuts' }
SpDropListButton >> selectedItem [
	self dropList selectedItem
]

{ #category : #'api-shortcuts' }
SpDropListButton >> setIndex: anIndex [
	self dropList setIndex: anIndex
]

{ #category : #'events-shortcuts' }
SpDropListButton >> whenSelectedItemChanged: aBlock [
	self dropList
		whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpDropListButton >> whenSelectionChanged: aBlock [
	self dropList
		whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpDropListButton >> whenSelectionIndexChanged: aBlock [
	self dropList
		whenSelectionIndexChanged: aBlock
]
