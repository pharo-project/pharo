"
A ListPresenter is an applicative presenter which handle a basic list.
See AbstractWidgetPresenter

self example
self example2
self example3

You can also have a look at ExampleListPresenter >> exampleRegisteredColor and ListSelectionPresenter for more examples.

I provide the following variables and their accessors
- backgroundColorBlock should convert an item in the color in the background in the line of this item.
- displayBlock should convert an item in something that can be displayed in a list, use asStringOrText by default.
- filteringBlock should convert an item in a boolean representing if the item will be displayed, initialize with self defaultFilteringBlock.
- multiSelection is a boolean representing if several items can be selected in the same time, it have shortcut beMultipleSelection and beSingleSelection.
- sortingBlock should convert two items in a boolean representing if the first item must be place before the second, initialize with self defaultSortingBlock.

Blocks should be set before items.

I provide the following methods
- items: to set the list with the elements of the collection in parameter
- getIndex is the getter of the index of the selected item or the last selected item.
- getList is the getter of the list.
- listSize is the number of items.


todo
- allowToSelect
- autoDeselect
- menu
- listItems
- clickOnSelectedItem
- getSelectionStateFor:
- listElementAt:ifAbsent:
- refresh ...
- select ... , setSelected ...
- updateList

"
Class {
	#name : #ListPresenter,
	#superclass : #AbstractListPresenter,
	#instVars : [
		'headerTitle',
		'displayBlockValueHolder'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #specs }
ListPresenter class >> adapterName [

	^ #ListAdapter
]

{ #category : #example }
ListPresenter class >> example [
	<sampleInstance>
	^ self new
		items: Smalltalk globals allClasses;
		openWithSpec;
		yourself
]

{ #category : #example }
ListPresenter class >> exampleMultipleSelection [
	<sampleInstance>
	^ self new
		items: Smalltalk globals allClasses;
		beMultipleSelection;
		openWithSpec;
		yourself
]

{ #category : #example }
ListPresenter class >> exampleWithHeaderTitle [
	<sampleInstance>
	^ self new
		items: Smalltalk globals allClasses;
		headerTitle: 'Title';
		openWithSpec;
		yourself
]

{ #category : #specs }
ListPresenter class >> title [

	^ 'List'
]

{ #category : #api }
ListPresenter >> allowToSelect [
	"Return whether the list items can be selected or not"
	
	self deprecated: 'Should not use. Use #selectionMode instead'.
	^ allowToSelect value
]

{ #category : #api }
ListPresenter >> allowToSelect: aBoolean [
	"Set if the list items can be selected or not"

	self deprecated: 'Should not use. Use #selectionMode: instead'.
	allowToSelect value: aBoolean.
	aBoolean 
		ifFalse: [ self resetSelection ].
]

{ #category : #api }
ListPresenter >> autoDeselect [
	"Return whether the list items can be selected if it's already the selected item"
	"If true, clicking again on the selected item will unselect it"

	self deprecated: 'Should not use. This feature is not supported anymore'.	
	^ autoDeselect value
]

{ #category : #api }
ListPresenter >> autoDeselect: aBoolean [
	"Set if the list items can be selected again"

	self deprecated: 'Should not use. This feature is not supported anymore'.
	autoDeselect value: aBoolean
]

{ #category : #api }
ListPresenter >> backgroundColorBlock [
	"Return the block use to compute the background color for each item"

	self deprecated: 'Should not use. This feature is not supported anymore. This feature depends on the theme'
]

{ #category : #api }
ListPresenter >> backgroundColorBlock: aBlock [
	"Set the block used to compute an item background"

	self deprecated: 'Should not use. This feature is not supported anymore. This feature depends on the theme'
]

{ #category : #api }
ListPresenter >> backgroundColorFor: anItem at: index [

	self deprecated: 'Should not use. This feature is not supported anymore. This feature depends on the theme'.
	^ self backgroundColorBlock cull: anItem cull: index
]

{ #category : #private }
ListPresenter >> clickOnSelectedItem [
	| item |
	
	item := self selectedItem.
	(item notNil and: [ item respondsTo: #click ])
		ifTrue: [ ^ item click ]
		ifFalse: [ ^ false ]
]

{ #category : #api }
ListPresenter >> displayBlock [
	"Return the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"
	
	^ displayBlockValueHolder value
]

{ #category : #api }
ListPresenter >> displayBlock: aBlock [
	"<api: #block getter:#displayBlock registration: #whenDisplayBlockChanged:>"
	"Set the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"
	
	displayBlockValueHolder value: aBlock
]

{ #category : #'adapter-api' }
ListPresenter >> displayValueAt: anIndex [
	
	^ self displayValueOf: (model value at: anIndex)
]

{ #category : #'adapter-api' }
ListPresenter >> displayValueOf: anObject [

	^ self displayBlock value: anObject
]

{ #category : #testing }
ListPresenter >> hasHeaderTitle [
	
	^ headerTitle value isEmptyOrNil not
]

{ #category : #accessing }
ListPresenter >> headerTitle [
	^ headerTitle value
]

{ #category : #'as yet unclassified' }
ListPresenter >> headerTitle: aString [ 
	
	headerTitle value: aString
]

{ #category : #api }
ListPresenter >> hideHeaderTitle [
	
	headerTitle value: nil
]

{ #category : #api }
ListPresenter >> initializeValueHolders [
	
	super initializeValueHolders.
	displayBlockValueHolder := [ :object | object asStringOrText ] asValueHolder.
	headerTitle := nil asValueHolder
]

{ #category : #private }
ListPresenter >> prepareForFilteredDataSourceWith: items [

	model := items asValueHolder

]

{ #category : #private }
ListPresenter >> registerEvents [
	
	super registerEvents.
	displayBlockValueHolder whenChangedDo: [ 
		self withAdapterDo: #refreshWidgetList ].
	headerTitle whenChangedDo: [ 
		self withAdapterDo: #refreshWidgetHeaderTitle ]
]

{ #category : #api }
ListPresenter >> resetFilteringBlock [
	"<api: #do>"
	"Reset the filtering block with the default value which consists in showing everything"

	filteringBlockHolder value: self defaultFilteringBlock
]

{ #category : #private }
ListPresenter >> resetListSelection [
	"Reset the selection manager for multiple selection lists"
	
	multiSelectionHolder reset
]

{ #category : #api }
ListPresenter >> resetSortingBlock [
	"<api: #do>"
	"Reset the sortering block with the default value which consists in not sorting"

	sortingBlockHolder value: self defaultSortingBlock
]

{ #category : #api }
ListPresenter >> selectAll [

	self selection selectAll
]

{ #category : #private }
ListPresenter >> setSelectionStateFor: anIndex at: aBoolean [
	"Set the selection state of the item at index _anIndex_ in the case of multiple selection list"

	self flag: 'refactoring in progress'.
	(self allowToSelect and: [ self isMultipleSelection ])
		ifFalse: [ ^ self ].
	^ self selection
		selectIndexes:
			(self selection selectedIndexes value copyWith: anIndex) asSet
				asOrderedCollection
]

{ #category : #api }
ListPresenter >> updateList [

	self withAdapterDo: #refreshWidgetList.
	self unselectAll
]

{ #category : #'api-events' }
ListPresenter >> whenAllowToSelectChanged: aBlock [
	"Set a block to value when the value of allowToSelect has changed"
	
	allowToSelect whenChangedDo: aBlock
]

{ #category : #'api-events' }
ListPresenter >> whenAutoDeselectChanged: aBlock [
	"Set a block to value when the value of autoDeselect has changed"
	
	autoDeselect whenChangedDo: aBlock
]

{ #category : #'api-events' }
ListPresenter >> whenBackgroundColorBlockChanged: aBlock [

	self deprecated: 'Should not use. This feature is not supported anymore. This feature depends on the theme'.

]

{ #category : #'api-events' }
ListPresenter >> whenDisplayBlockChanged: aBlock [
	"Set a block to value when the filtering block has changed"
	
	displayBlockValueHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
ListPresenter >> whenFilteringBlockChanged: aBlock [
	"Set a block to value when the filtering block block has changed"
	
	filteringBlockHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
ListPresenter >> whenMenuChanged: aBlock [
	"Set a block to value when the menu block has changed"
	
	menuHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
ListPresenter >> whenSortingBlockChanged: aBlock [
	"Set a block to value when the sorting block has changed"
	
	sortingBlockHolder whenChangedDo: aBlock
]
