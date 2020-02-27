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
	#traits : 'SpTSearchable',
	#classTraits : 'SpTSearchable classTrait',
	#instVars : [
		'#allowToSelect => SpObservableSlot',
		'#autoDeselect => SpObservableSlot',
		'#headerTitle => SpObservableSlot',
		'#display => SpObservableSlot',
		'#icon => SpObservableSlot'
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
	item := self selection selectedItem.
	(item notNil and: [ item respondsTo: #click ])
		ifTrue: [ ^ item click ]
		ifFalse: [ ^ false ]
]

{ #category : #api }
SpListPresenter >> display [

	^ display
]

{ #category : #api }
SpListPresenter >> display: aBlock [

	display := aBlock
]

{ #category : #api }
SpListPresenter >> displayBlock [
	"Return the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"
	self
		deprecated: 'Please use #display instead'
		transformWith: '`@receiver displayBlock' 
						-> '`@receiver display'.
	^ display
]

{ #category : #api }
SpListPresenter >> displayBlock: aBlock [
	"Set the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"

	"Return the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"
	self
		deprecated: 'Please use #display: instead'
		transformWith: '`@receiver displayBlock: `@arg' 
						-> '`@receiver display: `@arg'.

	display := aBlock
]

{ #category : #'adapter-api' }
SpListPresenter >> displayValueAt: anIndex [
	^ self displayValueOf: (model at: anIndex)
]

{ #category : #'adapter-api' }
SpListPresenter >> displayValueOf: anObject [

	^ self display value: anObject
]

{ #category : #testing }
SpListPresenter >> hasHeaderTitle [
	^ headerTitle isEmptyOrNil not
]

{ #category : #testing }
SpListPresenter >> hasIcons [

	^ self icons notNil
]

{ #category : #accessing }
SpListPresenter >> headerTitle [
	^ headerTitle
]

{ #category : #accessing }
SpListPresenter >> headerTitle: aString [
	headerTitle := aString
]

{ #category : #api }
SpListPresenter >> hideHeaderTitle [
	headerTitle := nil
]

{ #category : #'widget API' }
SpListPresenter >> iconFor: anItem [
	^ self icons cull: anItem cull: self
]

{ #category : #accessing }
SpListPresenter >> icons [
	"Return the block which takes an item as argument and returns the icon to display in the list"

	^ icon
]

{ #category : #accessing }
SpListPresenter >> icons: aBlock [
	"Set a block which takes an item as argument and returns the icon to display in the list"

	icon := aBlock
]

{ #category : #initialization }
SpListPresenter >> initialize [

	super initialize.
	self initializeTSearchable.
	autoDeselect := true.
	allowToSelect := true.
	display := [ :object | object asStringOrText ]
]

{ #category : #private }
SpListPresenter >> prepareForFilteredDataSourceWith: items [
	model := items
]

{ #category : #private }
SpListPresenter >> registerEvents [
	super registerEvents.
	"Do not use whenAutoDeselectChangedDo: to avoid the deprecation warning."
	self property: #autoDeselect whenChangedDo: [ :aBoolean | self changed: #autoDeselect: with: {aBoolean} ].
	self whenDisplayBlockChangedDo: [ self withAdapterDo: #refreshWidgetList ].
	self property: #headerTitle whenChangedDo: [ self withAdapterDo: #refreshWidgetHeaderTitle ]
]

{ #category : #private }
SpListPresenter >> resetListSelection [
	self selectIndex: 0
]

{ #category : #api }
SpListPresenter >> resetSortingBlock [
	"Reset the sortering block with the default value which consists in not sorting"

	self model sortingBlock: nil
]

{ #category : #api }
SpListPresenter >> updateList [

	self withAdapterDo: #refreshWidgetList.
	self unselectAll
]

{ #category : #'api-events' }
SpListPresenter >> whenDisplayBlockChangedDo: aBlock [
	"Set a block to value when the filtering block has changed"

	self property: #display whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpListPresenter >> whenIconsChangedDo: aBlock [
	self property: #icon whenChangedDo: aBlock
]
