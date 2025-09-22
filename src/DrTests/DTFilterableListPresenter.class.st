"
I am a list presenter that can be filtered.

I also have a label.
"
Class {
	#name : 'DTFilterableListPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'listPresenter',
		'filterTextInput',
		'initialItems',
		'labelPresenter'
	],
	#category : 'DrTests-Spec',
	#package : 'DrTests',
	#tag : 'Spec'
}

{ #category : 'api - actions' }
DTFilterableListPresenter >> actions [

	^ self listPresenter actions
]

{ #category : 'api - actions' }
DTFilterableListPresenter >> actions: aCommandGroup [

	self listPresenter actions: aCommandGroup
]

{ #category : 'accessing' }
DTFilterableListPresenter >> allItems [

	^ initialItems
]

{ #category : 'accessing' }
DTFilterableListPresenter >> beEmpty [

	self items: #()
]

{ #category : 'accessing' }
DTFilterableListPresenter >> beMultipleSelection [

	^ self listPresenter beMultipleSelection
]

{ #category : 'accessing' }
DTFilterableListPresenter >> beSingleSelection [

	^ self listPresenter beSingleSelection
]

{ #category : 'accessing' }
DTFilterableListPresenter >> contextMenu: aBlock [

	^ self listPresenter contextMenu: aBlock
]

{ #category : 'layout' }
DTFilterableListPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 5;
		add: labelPresenter expand: false;
		add: listPresenter;
		add: filterTextInput expand: false;
		yourself
]

{ #category : 'accessing' }
DTFilterableListPresenter >> displayBlock: aBlock [

	^ self listPresenter display: aBlock
]

{ #category : 'api' }
DTFilterableListPresenter >> displayColor: aBlock [

	self listPresenter displayColor: aBlock
]

{ #category : 'accessing' }
DTFilterableListPresenter >> displayIcon: aFullBlockClosure [

	self listPresenter displayIcon: aFullBlockClosure
]

{ #category : 'private - actions' }
DTFilterableListPresenter >> ensureActions [

	^ self listPresenter ensureActions
]

{ #category : 'private' }
DTFilterableListPresenter >> filterList [
	"Filters the list according to the filterTextInput."

	self unselectAll.
	self filterStrings
		ifEmpty: [
			self listPresenter
				items: initialItems.
			^ self ].
	self listPresenter
		items:
			(initialItems
				select: [ :each |
					self filterStrings
						anySatisfy: [ :any | any match: (self listPresenter display value: each) ] ])
]

{ #category : 'accessing' }
DTFilterableListPresenter >> filterStrings [

	^ (self filterTextInput text splitOn: $|)
		  reject: #isEmpty
		  thenCollect: [ :pattern | '*' , pattern , '*' ]
]

{ #category : 'accessing' }
DTFilterableListPresenter >> filterTextInput [

	^ filterTextInput
]

{ #category : 'accessing' }
DTFilterableListPresenter >> help [

	^ labelPresenter help
]

{ #category : 'accessing' }
DTFilterableListPresenter >> help: aString [

	labelPresenter help: aString
]

{ #category : 'initialization' }
DTFilterableListPresenter >> initialize [

	super initialize.
	initialItems := #()
]

{ #category : 'initialization' }
DTFilterableListPresenter >> initializePresenters [

	labelPresenter := self newLabel.
	listPresenter := self newList.
	filterTextInput := self newTextInput
		placeholder: 'Filter...';
		whenTextChangedDo: [ self filterList ];
		yourself
]

{ #category : 'accessing' }
DTFilterableListPresenter >> items [

	^ self visibleItems
]

{ #category : 'accessing' }
DTFilterableListPresenter >> items: items [

	initialItems := items.
	self listPresenter items: items
]

{ #category : 'accessing' }
DTFilterableListPresenter >> label [

	^ labelPresenter label
]

{ #category : 'accessing' }
DTFilterableListPresenter >> label: aString [

	labelPresenter label: aString
]

{ #category : 'accessing' }
DTFilterableListPresenter >> labelPresenter [

	^ labelPresenter
]

{ #category : 'accessing' }
DTFilterableListPresenter >> listPresenter [

	^ listPresenter
]

{ #category : 'actions' }
DTFilterableListPresenter >> resetFilter [

	self filterTextInput text: ''.
	self listPresenter items: initialItems
]

{ #category : 'actions' }
DTFilterableListPresenter >> selectAll [

	^ self listPresenter selectAll
]

{ #category : 'accessing' }
DTFilterableListPresenter >> selectItems: aBlock [

	^ self listPresenter selectItems: aBlock
]

{ #category : 'accessing' }
DTFilterableListPresenter >> selectedItem [

	^ listPresenter selectedItem
]

{ #category : 'accessing' }
DTFilterableListPresenter >> selectedItems [

	^ self listPresenter selectedItems
]

{ #category : 'accessing' }
DTFilterableListPresenter >> sortingBlock: aBlock [

	^ self listPresenter sortingBlock: aBlock
]

{ #category : 'actions' }
DTFilterableListPresenter >> unselectAll [

	^ self listPresenter unselectAll
]

{ #category : 'accessing' }
DTFilterableListPresenter >> visibleItems [

	^ self listPresenter items
]

{ #category : 'events' }
DTFilterableListPresenter >> whenSelectionChangedDo: aBlock [

	^ self listPresenter whenSelectionChangedDo: aBlock
]
