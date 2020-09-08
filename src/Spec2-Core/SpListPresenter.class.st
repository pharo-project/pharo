"
A `ListPresenter` is an applicative presenter which handles a basic list displaying information about a domain list. 
For example a list presenter will display the name of a list of contact. It refines the API of `SpAbstractListPresenter` that is shared with `SpComponentListPresenter` (which is a list of presenter) and `SpTablePresenter` (which is a list with multiple columns in a sense). 

### Design point
There are two design points worth notice:

A list presenter has a strategy that encapsulates specific API and action related to selection (how to select, how many items can be selected, currently selected items...). Such strategy is accessed via the `selection` message. It returns instance of the subclasses of `SpAbstractSelectionMode`: `SpMultipleSelectionMode` and `SpSingleSelectionMode`. 
It means many fonctionality related to the selection (selected items, multiple items, ....) are defined in the API of the mode and not of the main class.
 

A list presenter does not offer the possibility to add one item in isolation instead you should use the `items:` setter to set the full list domain object. The responsibility to manage the items displayed by a list presenter is not the one of that list presenter but its users e.g, the presenter you will write and that instantiates and uses the list presenter. This is normal since there is no way that the list presenter could track changes into the domain object representing the list items.




### Examples

You can find examples below:
```
self example
self exampleMultipleSelection
self exampleWithHeaderTitle
self exampleWithIcons
```
You can also have a look at `SpListSelectionPresenter` for more examples: it displays a list and how two other components update their contents based on the currently selected item. 

### Transmissions
The following ports are available: 
- `#inputItemsPort` (default)
- `#outputSelectionPort` (default)
- `#outputActivationPort`

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
	"aBlock (a one argument block) will be applied on each of the list items and the result of the block will be used to display the item on the screen.
Here is the typical example:

initializePresenters
   ...
   fontFamilyList := self newList.
fontFamilyList display: [ :fontFamily | fontFamily familyName ]
   ..."

	display := aBlock
]

{ #category : #private }
SpListPresenter >> displayValueAt: anIndex [
	"Return the effective string that is displayed on the list. 
	Note that it is different from the item, because often an item is an object whose only a facette (e.g., name) is displayed. In such case displayValueAt: return the string of the displayed facette"

	^ self displayValueFor: (model at: anIndex)
]

{ #category : #private }
SpListPresenter >> displayValueFor: anObject [

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
	self 
		property: #autoDeselect 
		whenChangedDo: [ :aBoolean | self changed: #autoDeselect: with: {aBoolean} ].
	self whenDisplayBlockChangedDo: [ 
		self withAdapterDo: [ :anAdapter | anAdapter refreshWidgetList ] ].
	self 
		property: #headerTitle 
		whenChangedDo: [ 
			self withAdapterDo: [ :anAdapter | anAdapter refreshWidgetHeaderTitle ] ]
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

	self withAdapterDo: [ :anAdapter | anAdapter refreshWidgetList ].
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
