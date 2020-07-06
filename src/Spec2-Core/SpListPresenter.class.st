"
A ListPresenter is an applicative presenter which handle a basic list.
See AbstractWidgetPresenter

self example
self exampleMultipleSelection
self exampleWithHeaderTitle
self exampleWithIcons

You can also have a look at ExampleListPresenter >> exampleRegisteredColor and ListSelectionPresenter for more examples.

!! Transmissions
- #inputItemsPort (default)
- #outputSelectionPort (default)
- #outputActivationPort
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

{ #category : #'adapter-api' }
SpListPresenter >> displayValueAt: anIndex [
	^ self displayValueFor: (model at: anIndex)
]

{ #category : #'adapter-api' }
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
