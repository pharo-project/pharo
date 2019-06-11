Class {
	#name : #AbstractListPresenter,
	#superclass : #AbstractWidgetPresenter,
	#traits : 'TSpecHaveWrappingScrollBars',
	#classTraits : 'TSpecHaveWrappingScrollBars classTrait',
	#instVars : [
		'selectionMode',
		'multiSelectionHolder',
		'filteringBlockHolder',
		'autoDeselect',
		'itemFilterBlockHolder',
		'allowToSelect',
		'doubleClickActionHolder',
		'activationBlock',
		'activateOnSingleClick',
		'model',
		'contextMenuHolder',
		'scrollBars'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #api }
AbstractListPresenter >> activateOnDoubleClick [
	
	activateOnSingleClick := false
]

{ #category : #api }
AbstractListPresenter >> activateOnSingleClick [
	
	activateOnSingleClick := true
]

{ #category : #testing }
AbstractListPresenter >> activatesOnDoubleClick [
	
	^ activateOnSingleClick not
]

{ #category : #testing }
AbstractListPresenter >> activatesOnSingleClick [
	
	^ activateOnSingleClick
]

{ #category : #api }
AbstractListPresenter >> beMultipleSelection [
	self selectionMode: (SpecMultipleSelectionMode on: self)
]

{ #category : #api }
AbstractListPresenter >> beSingleSelection [
	
	self selectionMode: (SpecSingleSelectionMode on: self)
]

{ #category : #simulation }
AbstractListPresenter >> clickAtIndex: anIndex [

	self selectIndex: anIndex.

	activateOnSingleClick ifFalse: [ ^ self ].
	self doActivateAtIndex: anIndex
]

{ #category : #simulation }
AbstractListPresenter >> clickItem: anInteger [ 
	
	self selectIndex: anInteger
]

{ #category : #api }
AbstractListPresenter >> contextMenu [
	"Return the block used to defined the menu"
	
	^ contextMenuHolder value
]

{ #category : #api }
AbstractListPresenter >> contextMenu: aValuable [
	"Set the block used to defined the menu"
	
	contextMenuHolder value: aValuable
]

{ #category : #initialization }
AbstractListPresenter >> defaultFilteringBlock [ 
	^ [ :col | col ]
]

{ #category : #api }
AbstractListPresenter >> disableFilter [
	self itemFilterBlock: nil
]

{ #category : #private }
AbstractListPresenter >> doActivateAtIndex: anIndex [

	"Activate only if there is an item at that position"
	self model at: anIndex ifAbsent: [ ^ self ].
	
	activationBlock cull: ((SpecSingleSelectionMode on: self)
		basicSelectIndex: anIndex;
		yourself)
]

{ #category : #simulation }
AbstractListPresenter >> doubleClickAtIndex: anIndex [
	self selectIndex: anIndex.
	
	activateOnSingleClick ifTrue: [ ^ self ].
	self doActivateAtIndex: anIndex
]

{ #category : #api }
AbstractListPresenter >> enableItemBeginsWithFilter [
	self itemFilterBlock: [ :each :pattern | each asLowercase beginsWith: pattern asLowercase ]
]

{ #category : #api }
AbstractListPresenter >> enableItemSubstringFilter [
	self itemFilterBlock: [ :each :pattern | each asLowercase includesSubstring: pattern asLowercase ]
]

{ #category : #initialization }
AbstractListPresenter >> initialize [

	super initialize.
	
	activationBlock := [ ].
	
	self withScrollBars.
	self model: 	self newEmptyModel.

	self beSingleSelection.
	self activateOnDoubleClick.

	self initializeValueHolders.
	self registerEvents
]

{ #category : #initialization }
AbstractListPresenter >> initializeValueHolders [
	itemFilterBlockHolder := nil asValueHolder.
	doubleClickActionHolder := [ ] asValueHolder.
	contextMenuHolder := nil asValueHolder.
	allowToSelect := true asValueHolder.
	autoDeselect := true asValueHolder
]

{ #category : #testing }
AbstractListPresenter >> isMultipleSelection [
	
	^ selectionMode isMultipleSelection
]

{ #category : #accessing }
AbstractListPresenter >> itemAt: anInteger [ 
	
	^ self model at: anInteger
]

{ #category : #api }
AbstractListPresenter >> itemFilterBlock [
	^ itemFilterBlockHolder value
]

{ #category : #api }
AbstractListPresenter >> itemFilterBlock: aBlock [
	"This block will be used with the search field of the list to filter it with the user input dynamically."

	itemFilterBlockHolder value: aBlock
]

{ #category : #private }
AbstractListPresenter >> items [
	
	^ self model collection
]

{ #category : #api }
AbstractListPresenter >> items: aCollection [
	"Set the items of the list.
	aCollection is a collection of your domain specific items.
	This creates a collection model"
	
	model collection: aCollection
]

{ #category : #private }
AbstractListPresenter >> listElementAt: anIndex [
	"Return the item at index _anIndex_"

	^ self model at: anIndex ifAbsent: [ nil ]
]

{ #category : #private }
AbstractListPresenter >> listElementAt: anIndex ifAbsent: aBlock [	
	"Return the item at index _anIndex_"
	
	^ self listItems at: anIndex ifAbsent: aBlock
]

{ #category : #api }
AbstractListPresenter >> listSize [
	"<api: #inspect>"

	"Return the size of the list"

	^ self model size
]

{ #category : #accessing }
AbstractListPresenter >> model [
	
	^ model
]

{ #category : #accessing }
AbstractListPresenter >> model: aModel [
	
	model := aModel.
	model whenChangedDo: [ 
		self withAdapterDo: #refreshList ]
]

{ #category : #private }
AbstractListPresenter >> newEmptyModel [

	^ SpecCollectionListModel on: #()
]

{ #category : #initialization }
AbstractListPresenter >> registerEvents [

	selectionMode whenChangedDo: [ :selection | 
		self withAdapterDo: #refreshWidgetSelection ].
	autoDeselect whenChangedDo: [ :aBoolean | 
		self changed: #autoDeselect: with: { aBoolean } ].
	contextMenuHolder whenChangedDo: [ 
		self withAdapterDo: #updateMenu ]
]

{ #category : #simulation }
AbstractListPresenter >> rightClickAtIndex: anIndex [
	
	self clickItem: anIndex.
	self halt.
	^ self menu: nil shifted: false
]

{ #category : #api }
AbstractListPresenter >> selectAll [

	self selection selectAll
]

{ #category : #api }
AbstractListPresenter >> selectIndex: anInteger [ 
	
	self selectionMode selectIndex: anInteger
]

{ #category : #api }
AbstractListPresenter >> selectIndexes: aCollection [
	self selectionMode selectIndexes: aCollection
]

{ #category : #api }
AbstractListPresenter >> selectItem: anItem [ 
	
	self selectionMode selectItem: anItem
]

{ #category : #api }
AbstractListPresenter >> selectItems: aCollection [
	self selectionMode selectItems: aCollection
]

{ #category : #api }
AbstractListPresenter >> selectedItems [
	"<api: #inspect>"

	"Return all the selected items in the case of a multiple selection list"

	^ self selectedItemsAtIndexes: self selection selectedIndexes
]

{ #category : #private }
AbstractListPresenter >> selectedItemsAtIndexes: aCollectionOfIndex [
	^ aCollectionOfIndex collect: [ :anIndex | self listElementAt: anIndex ]
]

{ #category : #api }
AbstractListPresenter >> selectedItemsSorted [
	"return all the selected items sorted by their index"

	^ self selectedItemsAtIndexes: self selection selectedIndexes sort
]

{ #category : #accessing }
AbstractListPresenter >> selection [
	
	^ selectionMode
]

{ #category : #accessing }
AbstractListPresenter >> selectionMode [
	
	^ selectionMode
]

{ #category : #api }
AbstractListPresenter >> selectionMode: aMode [
	
	selectionMode ifNotNil: [ 
		selectionMode transferSubscriptionsTo: aMode ].
	selectionMode := aMode.
]

{ #category : #api }
AbstractListPresenter >> sortingBlock [
	^ self model sortingBlock
]

{ #category : #api }
AbstractListPresenter >> sortingBlock: aBlock [
	self model sortingBlock: aBlock
]

{ #category : #api }
AbstractListPresenter >> unselectAll [
	
	self selection unselectAll
]

{ #category : #api }
AbstractListPresenter >> unselectIndex: anInteger [ 
	
	self selection unselectIndex: anInteger
]

{ #category : #api }
AbstractListPresenter >> unselectItem: anInteger [ 
	
	self selection unselectItem: anInteger
]

{ #category : #'api-events' }
AbstractListPresenter >> whenActivatedDo: aBlockClosure [ 
	
	activationBlock := aBlockClosure.
]

{ #category : #'api-events' }
AbstractListPresenter >> whenItemFilterBlockChangedDo: aBlock [
	itemFilterBlockHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractListPresenter >> whenMenuChangedDo: aBlock [
	"Set a block to value when the menu block has changed"
	
	contextMenuHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractListPresenter >> whenModelChangedDo: aBlock [

	model whenChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSelectionChangedDo: aBlock [
	"Subscribe to changes in selection.
	This method works for all selection modes (single, multiple and no selection).
	The block used as argument will be sent an optional argument with the selection object."

	self selection whenChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSortingBlockChangedDo: aBlock [
	self model whenSortingBlockChangedDo: aBlock
]
