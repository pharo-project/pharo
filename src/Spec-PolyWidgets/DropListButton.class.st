"
I am an elementary Spec widget that associates a DropListPresenter and a ButtonPresenter.

self example

I provide accessors dropList and input and shortcuts for the most common methods of their api.
"
Class {
	#name : #DropListButton,
	#superclass : #ComposablePresenter,
	#instVars : [
		'dropList',
		'button'
	],
	#category : 'Spec-PolyWidgets-Elementary'
}

{ #category : #specs }
DropListButton class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #dropList origin: 0 @ 0 corner: 0.8 @ 1;
				add: #button origin: 0.8 @ 0 corner: 1 @ 1 ];
		yourself
]

{ #category : #example }
DropListButton class >> example [
	| example |
	example := self new.
	example
		displayBlock: [ :item | item asString ];
		items: {'Swordian' . 'Gardian' . 'Wizard' . 'Sniper'};
		label: 'Add';
		extent: 300 @ 70;
		title: 'DropListButton example';
		openWithSpec.
	^ example
]

{ #category : #api-shortcuts }
DropListButton >> action: aBlock [
	self button action: aBlock
]

{ #category : #accessing }
DropListButton >> button [
	^ button
]

{ #category : #api-shortcuts }
DropListButton >> displayBlock: aBlock [
	self dropList displayBlock: aBlock
]

{ #category : #accessing }
DropListButton >> dropList [
	^ dropList
]

{ #category : #api-shortcuts }
DropListButton >> emptyList [
	self dropList emptyList
]

{ #category : #api-shortcuts }
DropListButton >> icon: anIcon [
	self button icon: anIcon
]

{ #category : #initialization }
DropListButton >> initializeWidgets [
	dropList := self instantiate: DropListPresenter.
	button := self instantiate: ButtonPresenter.
	self focusOrder
		add: dropList;
		add: button
]

{ #category : #api-shortcuts }
DropListButton >> items: aList [
	self dropList items: aList
]

{ #category : #api-shortcuts }
DropListButton >> label [
	self button label
]

{ #category : #api-shortcuts }
DropListButton >> label: aStringOrImageMorph [
	self button label: aStringOrImageMorph
]

{ #category : #api-shortcuts }
DropListButton >> listSize [
	self dropList listSize
]

{ #category : #api-shortcuts }
DropListButton >> resetSelection [
	self dropList resetSelection
]

{ #category : #api-shortcuts }
DropListButton >> selectedItem [
	self dropList selectedItem
]

{ #category : #api-shortcuts }
DropListButton >> setIndex: anIndex [
	self dropList setIndex: anIndex
]

{ #category : #events-shortcuts }
DropListButton >> whenSelectedItemChanged: aBlock [
	self dropList
		whenSelectedItemChanged: aBlock
]

{ #category : #events-shortcuts }
DropListButton >> whenSelectionChanged: aBlock [
	self dropList
		whenSelectionChanged: aBlock
]

{ #category : #events-shortcuts }
DropListButton >> whenSelectionIndexChanged: aBlock [
	self dropList
		whenSelectionIndexChanged: aBlock
]
