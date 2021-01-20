"
A presenter to display tree of elements.
A `SpTreePresenter` will display a line with a description of the element (See `SpTreePresenter>>#display:`, and optionally an icon (See `SpTreePresenter>>#icon:`).

A `SpTreePresenter` can be seen as a simplified `SpTreeTablePresenter`.

"
Class {
	#name : #SpTreePresenter,
	#superclass : #SpAbstractTreePresenter,
	#instVars : [
		'#showColumnHeaders => SpObservableSlot',
		'#isResizable => SpObservableSlot',
		'#iconBlock',
		'#displayBlock'
	],
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #specs }
SpTreePresenter class >> adapterName [

	^ #TreeAdapter
]

{ #category : #documentation }
SpTreePresenter class >> documentFactoryMethodSelector [

	^ #newTree
]

{ #category : #documentation }
SpTreePresenter class >> documentSections [
		
	^ OrderedDictionary newFromPairs: {
		'Examples'. self class methods select: [ :each | each protocol = self documentExamplesProtocol ].
		'API Methods'. self methods select: [ :each | each protocol = #'api' ].
		'API Selection Methods'. self methods select: [ :each | each protocol = #'api-selection' ].
		'Testing Methods'. self methods select: [ :each | each protocol = #'testing' ].
		'Events'. self methods select: [ :each | each protocol = #'api-events' ].
	 }
]

{ #category : #api }
SpTreePresenter >> beNotResizable [
	"Mark the table as 'not resizable', which means there will be not possibility to resize the 
	 columns of it."

	self isResizable: false
]

{ #category : #api }
SpTreePresenter >> beResizable [
	"Mark the table as 'resizable', which means there will be a slider to resize the columns."

	self isResizable: true
]

{ #category : #api }
SpTreePresenter >> display [
	"Answer the display block that will transform the model nodes into a displayable string."

	^ displayBlock
]

{ #category : #api }
SpTreePresenter >> display: aBlock [
	"Set the block that will be applied on each of the list items. 
	 The result of the block will be used to display the item on the screen.
	 `aBlock` receives one argument.
	 Here is the typical example:

	 initializePresenters
   		...
   		tree := self newTree.
		tree display: [ :anObject | anObject printString ]
   		...
	 "

	displayBlock := aBlock
]

{ #category : #api }
SpTreePresenter >> displayIcon [
	"Return the block used to return an icon that will be displayed in the tree"

	^ iconBlock
]

{ #category : #api }
SpTreePresenter >> displayIcon: aBlock [
	"Set a block which takes an item as argument and returns the icon to display in the tree.
	 `aBlock` receives one argument"
	
	iconBlock := aBlock
]

{ #category : #api }
SpTreePresenter >> hideColumnHeaders [
	"Hide the column headers"

	showColumnHeaders := false
]

{ #category : #initialization }
SpTreePresenter >> initialize [
	super initialize.

	self withScrollBars.
	
	activationBlock := [ ].
	
	displayBlock := [ :each | each asString ].
	showColumnHeaders := false.
	isResizable := false.
	childrenBlock := [ :item | #() ].
	roots := #().
	verticalAlignment := SpVerticalAlignment new.

	self beSingleSelection.
	self activateOnDoubleClick.
	self registerEvents
]

{ #category : #testing }
SpTreePresenter >> isResizable [
	"Answer true if table allows resizing of its columns."

	^ isResizable
]

{ #category : #private }
SpTreePresenter >> isResizable: aBoolean [
	isResizable := aBoolean
]

{ #category : #testing }
SpTreePresenter >> isShowingColumnHeaders [
	"Answer true if the table is configured to show column headers."

	^ showColumnHeaders
]

{ #category : #private }
SpTreePresenter >> shouldLazilyComputeChildren [

	^ false
]

{ #category : #api }
SpTreePresenter >> showColumnHeaders [
	"Show column headers"

	showColumnHeaders := true
]
