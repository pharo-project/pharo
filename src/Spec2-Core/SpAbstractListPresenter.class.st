Class {
	#name : #SpAbstractListPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#traits : 'TSpHaveWrappingScrollBars',
	#classTraits : 'TSpHaveWrappingScrollBars classTrait',
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
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #api }
SpAbstractListPresenter >> activateOnDoubleClick [
	
	activateOnSingleClick := false
]

{ #category : #api }
SpAbstractListPresenter >> activateOnSingleClick [
	
	activateOnSingleClick := true
]

{ #category : #testing }
SpAbstractListPresenter >> activatesOnDoubleClick [
	
	^ activateOnSingleClick not
]

{ #category : #testing }
SpAbstractListPresenter >> activatesOnSingleClick [
	
	^ activateOnSingleClick
]

{ #category : #api }
SpAbstractListPresenter >> beMultipleSelection [
	self selectionMode: (SpMultipleSelectionMode on: self)
]

{ #category : #api }
SpAbstractListPresenter >> beSingleSelection [
	
	self selectionMode: (SpSingleSelectionMode on: self)
]

{ #category : #simulation }
SpAbstractListPresenter >> clickAtIndex: anIndex [

	self selectIndex: anIndex.

	activateOnSingleClick ifFalse: [ ^ self ].
	self doActivateAtIndex: anIndex
]

{ #category : #simulation }
SpAbstractListPresenter >> clickItem: anInteger [ 
	
	self selectIndex: anInteger
]

{ #category : #api }
SpAbstractListPresenter >> contextMenu [
	"Return the block used to defined the menu"
	
	^ contextMenuHolder value
]

{ #category : #api }
SpAbstractListPresenter >> contextMenu: aValuable [
	"Set the block used to defined the menu"
	
	contextMenuHolder value: aValuable
]

{ #category : #initialization }
SpAbstractListPresenter >> defaultFilteringBlock [ 
	^ [ :col | col ]
]

{ #category : #api }
SpAbstractListPresenter >> disableFilter [
	self itemFilterBlock: nil
]

{ #category : #private }
SpAbstractListPresenter >> doActivateAtIndex: anIndex [

	"Activate only if there is an item at that position"
	self model at: anIndex ifAbsent: [ ^ self ].
	
	activationBlock cull: ((SpSingleSelectionMode on: self)
		basicSelectIndex: anIndex;
		yourself)
]

{ #category : #simulation }
SpAbstractListPresenter >> doubleClickAtIndex: anIndex [
	self selectIndex: anIndex.
	
	activateOnSingleClick ifTrue: [ ^ self ].
	self doActivateAtIndex: anIndex
]

{ #category : #api }
SpAbstractListPresenter >> enableItemBeginsWithFilter [
	self itemFilterBlock: [ :each :pattern | each asLowercase beginsWith: pattern asLowercase ]
]

{ #category : #api }
SpAbstractListPresenter >> enableItemSubstringFilter [
	self itemFilterBlock: [ :each :pattern | each asLowercase includesSubstring: pattern asLowercase ]
]

{ #category : #initialization }
SpAbstractListPresenter >> initialize [

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
SpAbstractListPresenter >> initializeValueHolders [
	itemFilterBlockHolder := nil asValueHolder.
	doubleClickActionHolder := [ ] asValueHolder.
	contextMenuHolder := nil asValueHolder.
	allowToSelect := true asValueHolder.
	autoDeselect := true asValueHolder
]

{ #category : #testing }
SpAbstractListPresenter >> isMultipleSelection [
	
	^ selectionMode isMultipleSelection
]

{ #category : #accessing }
SpAbstractListPresenter >> itemAt: anInteger [ 
	
	^ self model at: anInteger
]

{ #category : #api }
SpAbstractListPresenter >> itemFilterBlock [
	^ itemFilterBlockHolder value
]

{ #category : #api }
SpAbstractListPresenter >> itemFilterBlock: aBlock [
	"This block will be used with the search field of the list to filter it with the user input dynamically."

	itemFilterBlockHolder value: aBlock
]

{ #category : #private }
SpAbstractListPresenter >> items [
	
	^ self model collection
]

{ #category : #api }
SpAbstractListPresenter >> items: aCollection [
	"Set the items of the list.
	aCollection is a collection of your domain specific items.
	This creates a collection model"
	
	model collection: aCollection
]

{ #category : #private }
SpAbstractListPresenter >> listElementAt: anIndex [
	"Return the item at index _anIndex_"

	^ self model at: anIndex ifAbsent: [ nil ]
]

{ #category : #private }
SpAbstractListPresenter >> listElementAt: anIndex ifAbsent: aBlock [	
	"Return the item at index _anIndex_"
	
	^ self listItems at: anIndex ifAbsent: aBlock
]

{ #category : #api }
SpAbstractListPresenter >> listSize [
	"<api: #inspect>"

	"Return the size of the list"

	^ self model size
]

{ #category : #accessing }
SpAbstractListPresenter >> model [
	
	^ model
]

{ #category : #accessing }
SpAbstractListPresenter >> model: aModel [
	
	model := aModel.
	model whenChangedDo: [ 
		self withAdapterDo: #refreshList ]
]

{ #category : #private }
SpAbstractListPresenter >> newEmptyModel [

	^ SpCollectionListModel on: #()
]

{ #category : #initialization }
SpAbstractListPresenter >> registerEvents [

	selectionMode whenChangedDo: [ :selection | 
		self withAdapterDo: #refreshWidgetSelection ].
	autoDeselect whenChangedDo: [ :aBoolean | 
		self changed: #autoDeselect: with: { aBoolean } ].
	contextMenuHolder whenChangedDo: [ 
		self withAdapterDo: #updateMenu ]
]

{ #category : #simulation }
SpAbstractListPresenter >> rightClickAtIndex: anIndex [
	
	self clickItem: anIndex.
	self halt.
	^ self menu: nil shifted: false
]

{ #category : #api }
SpAbstractListPresenter >> selectAll [

	self selection selectAll
]

{ #category : #api }
SpAbstractListPresenter >> selectIndex: anInteger [ 
	
	self selectionMode selectIndex: anInteger
]

{ #category : #api }
SpAbstractListPresenter >> selectIndexes: aCollection [
	self selectionMode selectIndexes: aCollection
]

{ #category : #api }
SpAbstractListPresenter >> selectItem: anItem [ 
	
	self selectionMode selectItem: anItem
]

{ #category : #api }
SpAbstractListPresenter >> selectItems: aCollection [
	self selectionMode selectItems: aCollection
]

{ #category : #api }
SpAbstractListPresenter >> selectedItems [
	"<api: #inspect>"

	"Return all the selected items in the case of a multiple selection list"

	^ self selectedItemsAtIndexes: self selection selectedIndexes
]

{ #category : #private }
SpAbstractListPresenter >> selectedItemsAtIndexes: aCollectionOfIndex [
	^ aCollectionOfIndex collect: [ :anIndex | self listElementAt: anIndex ]
]

{ #category : #api }
SpAbstractListPresenter >> selectedItemsSorted [
	"return all the selected items sorted by their index"

	^ self selectedItemsAtIndexes: self selection selectedIndexes sort
]

{ #category : #accessing }
SpAbstractListPresenter >> selection [
	
	^ selectionMode
]

{ #category : #accessing }
SpAbstractListPresenter >> selectionMode [
	
	^ selectionMode
]

{ #category : #api }
SpAbstractListPresenter >> selectionMode: aMode [
	
	selectionMode ifNotNil: [ 
		selectionMode transferSubscriptionsTo: aMode ].
	selectionMode := aMode.
]

{ #category : #api }
SpAbstractListPresenter >> sortingBlock [
	^ self model sortingBlock
]

{ #category : #api }
SpAbstractListPresenter >> sortingBlock: aBlock [
	self model sortingBlock: aBlock
]

{ #category : #api }
SpAbstractListPresenter >> unselectAll [
	
	self selection unselectAll
]

{ #category : #api }
SpAbstractListPresenter >> unselectIndex: anInteger [ 
	
	self selection unselectIndex: anInteger
]

{ #category : #api }
SpAbstractListPresenter >> unselectItem: anInteger [ 
	
	self selection unselectItem: anInteger
]

{ #category : #'api-events' }
SpAbstractListPresenter >> whenActivatedDo: aBlockClosure [ 
	
	activationBlock := aBlockClosure.
]

{ #category : #'api-events' }
SpAbstractListPresenter >> whenItemFilterBlockChangedDo: aBlock [
	itemFilterBlockHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractListPresenter >> whenMenuChangedDo: aBlock [
	"Set a block to value when the menu block has changed"
	
	contextMenuHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractListPresenter >> whenModelChangedDo: aBlock [

	model whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractListPresenter >> whenSelectionChangedDo: aBlock [
	"Subscribe to changes in selection.
	This method works for all selection modes (single, multiple and no selection).
	The block used as argument will be sent an optional argument with the selection object."

	self selection whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractListPresenter >> whenSortingBlockChangedDo: aBlock [
	self model whenSortingBlockChangedDo: aBlock
]
