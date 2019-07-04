"
A ListPresenter is an applicative presenter which handle a basic list.
See AbstractWidgetPresenter

self example
self exampleMultipleSelection
self exampleWithHeaderTitle
self exampleWithIcons

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
	#name : #SpListPresenter,
	#superclass : #SpAbstractListPresenter,
	#instVars : [
		'headerTitle',
		'displayBlockValueHolder',
		'iconHolder'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #specs }
SpListPresenter class >> adapterName [

	^ #ListAdapter
]

{ #category : #specs }
SpListPresenter class >> title [

	^ 'List'
]

{ #category : #private }
SpListPresenter >> clickOnSelectedItem [
	| item |
	
	item := self selectedItem.
	(item notNil and: [ item respondsTo: #click ])
		ifTrue: [ ^ item click ]
		ifFalse: [ ^ false ]
]

{ #category : #api }
SpListPresenter >> displayBlock [
	"Return the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"
	
	^ displayBlockValueHolder value
]

{ #category : #api }
SpListPresenter >> displayBlock: aBlock [
	"<api: #block getter:#displayBlock registration: #whenDisplayBlockChanged:>"
	"Set the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"
	
	displayBlockValueHolder value: aBlock
]

{ #category : #'adapter-api' }
SpListPresenter >> displayValueAt: anIndex [
	
	^ self displayValueOf: (model value at: anIndex)
]

{ #category : #'adapter-api' }
SpListPresenter >> displayValueOf: anObject [

	^ self displayBlock value: anObject
]

{ #category : #testing }
SpListPresenter >> hasHeaderTitle [
	
	^ headerTitle value isEmptyOrNil not
]

{ #category : #accessing }
SpListPresenter >> headerTitle [
	^ headerTitle value
]

{ #category : #accessing }
SpListPresenter >> headerTitle: aString [ 
	
	headerTitle value: aString
]

{ #category : #api }
SpListPresenter >> hideHeaderTitle [
	
	headerTitle value: nil
]

{ #category : #'widget API' }
SpListPresenter >> iconFor: anItem [
	^ iconHolder value cull: anItem cull: self
]

{ #category : #accessing }
SpListPresenter >> icons [
	"Return the block which takes an item as argument and returns the icon to display in the list"

	^ iconHolder value
]

{ #category : #accessing }
SpListPresenter >> icons: aBlock [
	"Set a block which takes an item as argument and returns the icon to display in the list"

	iconHolder value: aBlock
]

{ #category : #initialization }
SpListPresenter >> initialize [

	super initialize.
	self
		bindKeyCombination: Character space
		toAction: [ self clickOnSelectedItem ]	
]

{ #category : #api }
SpListPresenter >> initializeValueHolders [
	super initializeValueHolders.
	displayBlockValueHolder := [ :object | object asStringOrText ] asValueHolder.
	headerTitle := nil asValueHolder.
	iconHolder := [ :item | nil ] asValueHolder
]

{ #category : #private }
SpListPresenter >> prepareForFilteredDataSourceWith: items [

	model := items asValueHolder

]

{ #category : #private }
SpListPresenter >> registerEvents [
	
	super registerEvents.
	displayBlockValueHolder whenChangedDo: [ 
		self withAdapterDo: #refreshWidgetList ].
	headerTitle whenChangedDo: [ 
		self withAdapterDo: #refreshWidgetHeaderTitle ]
]

{ #category : #api }
SpListPresenter >> resetFilteringBlock [
	"<api: #do>"
	"Reset the filtering block with the default value which consists in showing everything"

	filteringBlockHolder value: self defaultFilteringBlock
]

{ #category : #private }
SpListPresenter >> resetListSelection [
	"Reset the selection manager for multiple selection lists"
	
	multiSelectionHolder reset
]

{ #category : #api }
SpListPresenter >> resetSortingBlock [
	"Reset the sortering block with the default value which consists in not sorting"

	self model sortingBlock: nil
]

{ #category : #private }
SpListPresenter >> setSelectionStateFor: anIndex at: aBoolean [
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
SpListPresenter >> updateList [

	self withAdapterDo: #refreshWidgetList.
	self unselectAll
]

{ #category : #'api-events' }
SpListPresenter >> whenAllowToSelectChangedDo: aBlock [
	"Set a block to value when the value of allowToSelect has changed"
	
	allowToSelect whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpListPresenter >> whenAutoDeselectChangedDo: aBlock [
	"Set a block to value when the value of autoDeselect has changed"
	
	autoDeselect whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpListPresenter >> whenDisplayBlockChangedDo: aBlock [
	"Set a block to value when the filtering block has changed"
	
	displayBlockValueHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpListPresenter >> whenFilteringBlockChangedDo: aBlock [
	"Set a block to value when the filtering block block has changed"
	
	filteringBlockHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpListPresenter >> whenIconsChangedDo: aBlock [
	iconHolder whenChangedDo: aBlock
]
