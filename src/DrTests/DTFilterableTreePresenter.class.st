"
I am a list presenter that can be filtered.

I also have a label.
"
Class {
	#name : 'DTFilterableTreePresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'treePresenter',
		'filterTextInput',
		'initialItems',
		'labelPresenter'
	],
	#category : 'DrTests-Spec',
	#package : 'DrTests',
	#tag : 'Spec'
}

{ #category : 'api - actions' }
DTFilterableTreePresenter >> actions [

	^ self treePresenter actions
]

{ #category : 'api - actions' }
DTFilterableTreePresenter >> actions: aCommandGroup [

	self treePresenter actions: aCommandGroup
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> allItems [

	^ initialItems
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> beEmpty [

	self roots: #()
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> beMultipleSelection [

	^ self treePresenter beMultipleSelection
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> beSingleSelection [

	^ self treePresenter beSingleSelection
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> children [

	^ self treePresenter children
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> children: aBlock [

	self treePresenter children: aBlock
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> contextMenu: aBlock [

	^ self treePresenter contextMenu: aBlock
]

{ #category : 'layout' }
DTFilterableTreePresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		spacing: 5;
		add: labelPresenter expand: false;
		add: treePresenter;
		add: filterTextInput expand: false;
		yourself
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> displayBlock: aBlock [

	^ self treePresenter display: aBlock
]

{ #category : 'api' }
DTFilterableTreePresenter >> displayColor: aBlock [

	self treePresenter displayColor: aBlock
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> displayIcon: aFullBlockClosure [

	self treePresenter displayIcon: aFullBlockClosure
]

{ #category : 'private - actions' }
DTFilterableTreePresenter >> ensureActions [

	^ self treePresenter ensureActions
]

{ #category : 'expanding-collapsing' }
DTFilterableTreePresenter >> expandAll [

	self treePresenter expandAll
]

{ #category : 'private' }
DTFilterableTreePresenter >> filterList [
	"Filters the list according to the filterTextInput."

	self unselectAll.
	self filterStrings ifEmpty: [
			self roots: initialItems.
			^ self ].
	self roots: (initialItems select: [ :each |
				 self filterStrings anySatisfy: [ :any |
					 any match: (self treePresenter display value: each) ] ])
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> filterStrings [

	^ (self filterTextInput text splitOn: $|)
		  reject: #isEmpty
		  thenCollect: [ :pattern | '*' , pattern , '*' ]
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> filterTextInput [

	^ filterTextInput
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> help [

	^ labelPresenter help
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> help: aString [

	labelPresenter help: aString
]

{ #category : 'initialization' }
DTFilterableTreePresenter >> initialize [

	super initialize.
	initialItems := #()
]

{ #category : 'initialization' }
DTFilterableTreePresenter >> initializePresenters [

	labelPresenter := self newLabel.
	treePresenter := self newTree.
	filterTextInput := self newTextInput
		                   placeholder: 'Filter...';
		                   whenTextChangedDo: [ self filterList ];
		                   yourself
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> items [

	^ self visibleItems
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> label [

	^ labelPresenter label
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> label: aString [

	labelPresenter label: aString
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> labelPresenter [

	^ labelPresenter
]

{ #category : 'actions' }
DTFilterableTreePresenter >> resetFilter [

	self filterTextInput text: ''.
	self treePresenter roots: initialItems
]

{ #category : 'api' }
DTFilterableTreePresenter >> roots: aBlock [

	initialItems := aBlock.
	self treePresenter roots: aBlock
]

{ #category : 'actions' }
DTFilterableTreePresenter >> selectAll [

	^ self treePresenter selectAll
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> selectItems: aBlock [

	^ self treePresenter selectItems: aBlock
]

{ #category : 'api - selection' }
DTFilterableTreePresenter >> selectPaths: aPathArray [

	self treePresenter selectPaths: aPathArray
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> selectedItems [

	^ self treePresenter selectedItems
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> treePresenter [

	^ treePresenter
]

{ #category : 'actions' }
DTFilterableTreePresenter >> unselectAll [

	^ self treePresenter unselectAll
]

{ #category : 'accessing' }
DTFilterableTreePresenter >> visibleItems [

	^ self treePresenter roots
]

{ #category : 'events' }
DTFilterableTreePresenter >> whenSelectionChangedDo: aBlock [

	^ self treePresenter whenSelectionChangedDo: aBlock
]
