Class {
	#name : #AbstractListPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'selectionMode',
		'multiSelectionHolder',
		'sortingBlockHolder',
		'filteringBlockHolder',
		'autoDeselect',
		'itemFilterBlockHolder',
		'menuHolder',
		'allowToSelect',
		'doubleClickActionHolder',
		'activationBlock',
		'activateOnSingleClick',
		'model'
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

{ #category : #initialization }
AbstractListPresenter >> defaultFilteringBlock [ 
	^ [:col | col]
]

{ #category : #initialization }
AbstractListPresenter >> defaultSortingBlock [ 
	^ [:a :b | true]
]

{ #category : #private }
AbstractListPresenter >> doActivateAtIndex: anIndex [

	"Activate only if there is an item at that position"
	self model at: anIndex ifAbsent: [ ^ self ].
	
	activationBlock cull: ((SpecSingleSelectionMode on: self)
		basicSelectIndex: anIndex;
		yourself)
]

{ #category : #'api-events' }
AbstractListPresenter >> doubleClickAction: aBlockClosure [ 
	
	self
		deprecated: 'Use whenActivatedDo: instead'
		transformWith: '`@receiver whenActivatedDo: `@argument'
						-> '`@receiver doubleClickAction: `@argument'.
	
	self whenActivatedDo: aBlockClosure
]

{ #category : #simulation }
AbstractListPresenter >> doubleClickAtIndex: anIndex [
	self selectIndex: anIndex.
	
	activateOnSingleClick ifTrue: [ ^ self ].
	self doActivateAtIndex: anIndex
]

{ #category : #api }
AbstractListPresenter >> filteringBlock [
	"<api: #inspect>"
	"Return the filtering of the items"
	
	^ filteringBlockHolder value
]

{ #category : #api }
AbstractListPresenter >> filteringBlock: aBlock [
	"<api: #block getter: #filteringBlock registration: #whenFilteringBlockChanged:>"
	"To set the filtering of the items"
	
	filteringBlockHolder value: aBlock
]

{ #category : #private }
AbstractListPresenter >> getSelectionStateFor: anIndex [
	"Return the current state of the item -if selected or not - in a multiple selection list"
	"Answer true if the item at index _anIndex_ is selected"
	
	self flag: 'refactoring in progress'.

	^ multiSelectionHolder indexes
]

{ #category : #initialization }
AbstractListPresenter >> initialize [
	super initialize.

	activationBlock := [ ].
	
	self model: 	self newEmptyModel.

	self beSingleSelection.
	self activateOnDoubleClick.

	self initializeValueHolders.
	self registerEvents.

	self
		bindKeyCombination: Character space
		toAction: [ self clickOnSelectedItem ]
]

{ #category : #initialization }
AbstractListPresenter >> initializePorts [

	self addOutputPort: ListSelectionPresenterPort new.
	self addInputPort: ItemsPresenterPort new
]

{ #category : #initialization }
AbstractListPresenter >> initializeValueHolders [
	sortingBlockHolder := self defaultSortingBlock asValueHolder.
	filteringBlockHolder := self defaultFilteringBlock asValueHolder.
	itemFilterBlockHolder := nil asValueHolder.
	doubleClickActionHolder := [  ] asValueHolder.
	menuHolder := [ :menu :shifted | nil ] asValueHolder.
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
AbstractListPresenter >> itemBeginsWithFilter [
	self itemFilterBlock: [ :each :pattern | each asLowercase beginsWith: pattern asLowercase ]
]

{ #category : #api }
AbstractListPresenter >> itemFilterBlock [
	^ itemFilterBlockHolder value
]

{ #category : #api }
AbstractListPresenter >> itemFilterBlock: aBlock [
	itemFilterBlockHolder value: aBlock
]

{ #category : #api }
AbstractListPresenter >> itemSubstringFilter [
	self itemFilterBlock: [ :each :pattern | each asLowercase includesSubstring: pattern asLowercase ]
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

	^ self model shownItems at: anIndex ifAbsent: [ nil ]
]

{ #category : #private }
AbstractListPresenter >> listElementAt: anIndex ifAbsent: aBlock [	
	"Return the item at index _anIndex_"
	
	^ self listItems at: anIndex ifAbsent: aBlock
]

{ #category : #api }
AbstractListPresenter >> listItems [
	
	self
		deprecated: 'Please use the #model instead'
		transformWith: '`@receiver listItems' 
						-> '`@receiver model shownItems'.	
	^ self model shownItems
]

{ #category : #api }
AbstractListPresenter >> listSize [
	"<api: #inspect>"

	"Return the size of the list"

	^ self model shownItems size
]

{ #category : #api }
AbstractListPresenter >> menu [
	"<api: #inspect>"
	"Return the block used to defined the menu"
	
	^ menuHolder value
]

{ #category : #api }
AbstractListPresenter >> menu: aBlock [
	"<api: #block getter: #menu registration: #whenMenuChanged:>"
	"Set the block used to defined the menu"
	
	menuHolder value: aBlock
]

{ #category : #private }
AbstractListPresenter >> menu: aMenu shifted: aBoolean [
	"Build the menu when you right click on an item"

	"This does not work on multiple selection"

	self flag: #bug.
	self selection selectedItems ifEmpty: [ ^ nil ].
	^ menuHolder value cull: aMenu cull: aBoolean
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
		self changed: #autoDeselect: with: { aBoolean }].
]

{ #category : #api }
AbstractListPresenter >> resetSelection [
	"Unselect every items"

	self
		deprecated: 'Please use #unselectAll instead'
		transformWith: '`@receiver resetSelection' 
						-> '`@receiver unselectAll'.

	self unselectAll
]

{ #category : #simulation }
AbstractListPresenter >> rightClickAtIndex: anIndex [
	
	self clickItem: anIndex.
	^ self menu: nil shifted: false
]

{ #category : #api }
AbstractListPresenter >> selectIndex: anInteger [ 
	
	self selectionMode selectIndex: anInteger
]

{ #category : #api }
AbstractListPresenter >> selectItem: anItem [ 
	
	self selectionMode selectItem: anItem
]

{ #category : #api }
AbstractListPresenter >> selectedIndex [
	"Return the index of the selected item
	In the case of a multiple selection list, it returns the last selected item"
	
	self
		deprecated: 'Use the #selection object instead. This method assumes single selection'
		transformWith: '`@receiver selectedIndex'
						-> '`@receiver selection selectedIndex'.
	
	^  self selection selectedIndex
]

{ #category : #api }
AbstractListPresenter >> selectedIndex: anIndex [

	"Set the index of the item you want to be selected"
	self
		deprecated: 'Use #selectIndex: instead'
		transformWith: '`@receiver selectedIndex: `@arg'
						-> '`@receiver selectIndex: `@arg'.
	
	self selectIndex: anIndex
]

{ #category : #api }
AbstractListPresenter >> selectedIndexes [
	self
		deprecated: 'Use the #selection object instead. This method assumes multiple selection'
		transformWith: '`@receiver selectedIndexes'
						-> '`@receiver selection selectedIndexes'.

	^  self selection selectedIndexes
]

{ #category : #api }
AbstractListPresenter >> selectedItem [
	"Return the selected item.
	In the case of a multiple selection list, it returns the last selected item"
	
	self
		deprecated: 'Use the #selection object instead. This method assumes single selection'
		transformWith: '`@receiver selectedItem'
						-> '`@receiver selection selectedItem'.
	
	^ self selection selectedItem
]

{ #category : #api }
AbstractListPresenter >> selectedItem: anItem [
	"Set the item you want to be selected"

	self
		deprecated: 'Use #selectItem: instead'
		transformWith: '`@receiver selectedItem: `@arg'
						-> '`@receiver selectItem: `@arg'.

	self selectItem: anItem
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

{ #category : #private }
AbstractListPresenter >> setIndex: anIndex [
	self
		deprecated: 'Use #selectIndex: instead'
		transformWith: '`@receiver setIndex: `@arg' -> '`@receiver selectIndex: `@arg'.
	self selectIndex: anIndex
]

{ #category : #private }
AbstractListPresenter >> setIndexes: aCollectionOfIndexes [
	"Set the indexexs of the selected items in case of multiple selection"
	self
		deprecated: 'Use the #selection object instead. This method assumes multiple selection'
		transformWith: '`@receiver setIndexes: `@arg'
						-> '`@receiver selection selectIndexes: `@arg'.

	self selection selectIndexes: aCollectionOfIndexes
]

{ #category : #api }
AbstractListPresenter >> setSelectedIndex: anIndex [
	self
		deprecated: 'Use #selectedIndex: instead'
		transformWith:
			'`@receiver setSelectedIndex: `@argument'
				-> '`@receiver selectedIndex: `@argument'.
	^ self selectIndex: anIndex
]

{ #category : #api }
AbstractListPresenter >> setSelectedItem: anIndex [
	self
		deprecated: 'Use #selectedItem: instead'
		transformWith:
			'`@receiver setSelectedItem: `@argument'
				-> '`@receiver selectedItem:`@argument'.
	^ self selectItem: anIndex
]

{ #category : #api }
AbstractListPresenter >> sortingBlock [
	"<api: #inspect>"
	"Return the ordering of the items"
	
	^ sortingBlockHolder value
]

{ #category : #api }
AbstractListPresenter >> sortingBlock: aBlock [
	"<api: #block getter: #sortingBlock registration: #whenSortingBlockChanged:>"
	"To set the ordering of the items"
	
	sortingBlockHolder value: aBlock
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
AbstractListPresenter >> whenListChanged: aBlock [
	"Specify a block to value after the contents of the list has changed"
	"Basically when you set a new list of items"
	"<api: #event>"
	self
		deprecated: 'Use #whenModelChangedDo: instead'
		transformWith: '`@receiver whenListChanged: `@argument'
						-> '`@receiver whenModelChangedDo: `@argument'.
	
	self whenModelChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractListPresenter >> whenModelChangedDo: aBlock [

	model whenChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractListPresenter >> whenMultiSelectionChanged: aBlock [
	"Set a block to value when the multiSelection value has changed"
	self deprecated: 'Should not use'
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSelectedIndexChangedDo: aBlock [
	"Set a block to value when the selection index has changed"
	
	self
		deprecated: 'Use #selection whenChangedDo: instead'
		transformWith: '`@receiver whenSelectedIndexChangedDo: `@argument'
						-> '`@receiver selection whenChangedDo: [ :selection | `@argument value: selection selectedIndex ]'.

	^ self  selection whenChangedDo: [ :selection | aBlock value: selection selectedIndex ]
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSelectedItemChanged: aBlock [
	self
		deprecated: 'Use #whenSelectedItemChangedDo: instead'
		transformWith:
			'`@receiver whenSelectedItemChanged: `@argument'
				-> '`@receiver whenSelectedItemChangedDo: `@argument'.
	^ self
		whenSelectionChangedDo: [ :selection | aBlock cull: selection selectedItem ]
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSelectedItemChangedDo: aBlock [
	"Set a block to value when the select item is changed"

	self
		deprecated: 'Use #whenSelectionChangedDo: instead'
		transformWith: '`@receiver whenSelectedItemChangedDo: `@argument'
						-> '`@receiver whenSelectionChangedDo: [ :selection | `@argument cull: selection selectedItem ]'.
	
	self selection whenChangedDo: [ :selection | aBlock cull: selection selectedItem ]
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSelectedItemsChanged: aBlock [
	"Set a block to value when the select item is changed"
	
	self
		deprecated: 'Use #whenSelectionChangedDo: instead'
		transformWith: '`@receiver whenSelectedItemsChanged: `@argument'
						-> '`@receiver whenSelectionChangedDo: [ :selection | `@argument cull: selection selectedItems ]'.

	self selection whenChangedDo: [ :selection | aBlock cull: selection selectedItems ]
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSelectionChanged: aBlock [
	
	self
		deprecated: 'Use #whenSelectionChangedDo: instead'
		transformWith: '`@receiver whenSelectionChanged: `@argument'
						-> '`@receiver whenSelectionChangedDo: `@argument'.
	^ self whenSelectionChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSelectionChangedDo: aBlock [
	"Subscribe to changes in selection.
	This method works for all selection modes (single, multiple and no selection).
	The block used as argument will be sent an optional argument with the selection object."

	self selection whenChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSelectionIndexChanged: aBlock [
	
	self
		deprecated: 'Use #selection whenChangedDo: instead'
		transformWith: '`@receiver whenSelectionIndexChanged: `@argument'
						-> '`@receiver selection whenChangedDo: [ :selection | `@argument value: selection selectedIndex ]'.
	^ self  selection whenChangedDo: [ :selection | aBlock value: selection selectedIndex ]
]

{ #category : #'api-events' }
AbstractListPresenter >> whenSelectionIndexesChanged: aBlock [
	"Set a block to value when the selection index has changed"
	
	self
		deprecated: 'Use #whenSelectionChangedDo: instead'
		transformWith: '`@receiver whenSelectionIndexesChanged: `@argument'
						-> '`@receiver whenSelectionChangedDo: [ :selection | `@argument value: selection selectedIndexes ]'.
	
	self whenSelectionChangedDo: [ :selection | aBlock value: selection selectedIndexes ].
]
