"
I am a list presenter that can be filtered.

I also have a label.
"
Class {
	#name : #DTFilterableListPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'listPresenter',
		'filterTextInput',
		'initialItems',
		'labelPresenter'
	],
	#category : #'DrTests-Spec'
}

{ #category : #specs }
DTFilterableListPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #labelPresenter withConstraints: [ :constraints | constraints height: self labelHeight ];
		add: #listPresenter;
		add: #filterTextInput withConstraints: [ :constraints | constraints height: self inputTextHeight ];
		yourself
]

{ #category : #accessing }
DTFilterableListPresenter >> allItems [
	^ initialItems
]

{ #category : #api }
DTFilterableListPresenter >> beEmpty [
	self items: #()
]

{ #category : #api }
DTFilterableListPresenter >> beMultipleSelection [
	^ self listPresenter beMultipleSelection
]

{ #category : #api }
DTFilterableListPresenter >> beSingleSelection [
	^ self listPresenter beSingleSelection
]

{ #category : #api }
DTFilterableListPresenter >> contextMenu: aBlock [
	^ self listPresenter contextMenu: aBlock
]

{ #category : #api }
DTFilterableListPresenter >> displayBlock: aBlock [
	^ self listPresenter display: aBlock
]

{ #category : #private }
DTFilterableListPresenter >> filterList [
	"Filters the list according to the filterTextInput."

	self unselectAll.
	self listPresenter
		items:
			(initialItems
				select:
					[ :each | self filterString match: (self listPresenter display value: each) ])
]

{ #category : #accessing }
DTFilterableListPresenter >> filterString [
	^ String streamContents: [ :stream |
		stream nextPut: $*.
		stream nextPutAll: self filterTextInput text.
		stream nextPut: $* ]
]

{ #category : #accessing }
DTFilterableListPresenter >> filterTextInput [
	^ filterTextInput
]

{ #category : #accessing }
DTFilterableListPresenter >> help [
	^ labelPresenter help
]

{ #category : #accessing }
DTFilterableListPresenter >> help: aString [
	labelPresenter help: aString
]

{ #category : #initialization }
DTFilterableListPresenter >> initialize [
	initialItems := #().
	super initialize.
]

{ #category : #initialization }
DTFilterableListPresenter >> initializeWidgets [
	labelPresenter := self newLabel.
	listPresenter := self newList.
	filterTextInput := self newTextInput
								placeholder: 'Filter...';
								whenTextChangedDo: [ self filterList ];
								yourself
]

{ #category : #accessing }
DTFilterableListPresenter >> items [
	^ self visibleItems
]

{ #category : #api }
DTFilterableListPresenter >> items: items [
	initialItems := items.
	self listPresenter items: items
]

{ #category : #accessing }
DTFilterableListPresenter >> label [
	^ labelPresenter label
]

{ #category : #accessing }
DTFilterableListPresenter >> label: aString [
	labelPresenter label: aString
]

{ #category : #accessing }
DTFilterableListPresenter >> labelPresenter [
	^ labelPresenter
]

{ #category : #accessing }
DTFilterableListPresenter >> listPresenter [
	^ listPresenter
]

{ #category : #api }
DTFilterableListPresenter >> resetFilter [
	self filterTextInput text: ''.
	self listPresenter items: initialItems.
]

{ #category : #api }
DTFilterableListPresenter >> selectAll [
	^ self listPresenter selectAll
]

{ #category : #api }
DTFilterableListPresenter >> selectItems: aBlock [
	^ self listPresenter selectItems: aBlock
]

{ #category : #api }
DTFilterableListPresenter >> selectedItems [
	^ self listPresenter selectedItems
]

{ #category : #api }
DTFilterableListPresenter >> sortingBlock: aBlock [
	^ self listPresenter sortingBlock: aBlock
]

{ #category : #api }
DTFilterableListPresenter >> unselectAll [
	^ self listPresenter unselectAll
]

{ #category : #accessing }
DTFilterableListPresenter >> visibleItems [
	^ self listPresenter items
]

{ #category : #api }
DTFilterableListPresenter >> whenSelectionChangedDo: aBlock [
	^ self listPresenter whenSelectionChangedDo: aBlock
]
