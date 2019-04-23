"
An FastTablePresenter is a spec presenter for a FastTable
"
Class {
	#name : #FastTablePresenter,
	#superclass : #ColumnedListPresenter,
	#instVars : [
		'iconHolder',
		'handlesDoubleClick',
		'displayBlock'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #example }
FastTablePresenter class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		icons: [ :e | example iconNamed: #forwardIcon ];
		items: (1 to: 10) asArray;
		openWithSpec.
	^ example
]

{ #category : #example }
FastTablePresenter class >> exampleMultipleSelection [
	<sampleInstance>
	| example |
	example := self new.
	example
		items: (1 to: 10);
		beMultipleSelection;
		whenSelectionChangedDo: [ :selection | selection logCr ];
		openWithSpec.
	^ example
]

{ #category : #'api-columns' }
FastTablePresenter >> addColumn: aColumn [

	aColumn table: self.
	super addColumn: aColumn.
]

{ #category : #initialization }
FastTablePresenter >> displayBlock [

	^ displayBlock
]

{ #category : #initialization }
FastTablePresenter >> displayBlock: aBlock [

	displayBlock := aBlock
]

{ #category : #api }
FastTablePresenter >> getIconFor: anItem [

	^ iconHolder value cull: anItem cull: self
]

{ #category : #api }
FastTablePresenter >> handlesDoubleClick [

	^ handlesDoubleClick value
]

{ #category : #api }
FastTablePresenter >> handlesDoubleClick: aBoolean [
	"<api: #block getter:#handlesDoubleClick registration: #whenHandlesDoubleClickChanged:>"
	"Enable or disable double click on the list."
	
	handlesDoubleClick value: aBoolean
]

{ #category : #api }
FastTablePresenter >> icons [
	"<api: #inspect>"
	"Return the block which takes an item as argument and returns the icon to display in the list"
	
	^ iconHolder value
]

{ #category : #api }
FastTablePresenter >> icons: aBlock [
	"<api: #block getter: #icon registration: #whenIconsChanged: >"
	"Set a block which takes an item as argument and returns the icon to display in the list"
	iconHolder value: aBlock
]

{ #category : #initialization }
FastTablePresenter >> initialize [

	displayBlock := #yourself.
	super initialize.
	self addColumn: SpecFTColumn new
]

{ #category : #initialization }
FastTablePresenter >> initializeValueHolders [

	super initializeValueHolders.

	iconHolder := [:item | nil ] asValueHolder.
	handlesDoubleClick := false asValueHolder.
]

{ #category : #'api-columns' }
FastTablePresenter >> numberOfColumns: aNumber [

	columns := OrderedCollection new.
	((1 to: aNumber) do: [ :index | 
		self addColumn: (SpecFTArrayIndexColumn index: index) ] )
]

{ #category : #'api-columns' }
FastTablePresenter >> numberOfColumns: aNumber withWidths: widths [

	columns := OrderedCollection new.
	((1 to: aNumber) do: [ :index | 
		self addColumn: (SpecFTArrayIndexColumn index: index) ] )
]

{ #category : #initialization }
FastTablePresenter >> registerEvents [
	super registerEvents.
	handlesDoubleClick whenChangedDo: [ :bool | self changed: { #handlesDoubleClick . bool } ].

]

{ #category : #'api-events' }
FastTablePresenter >> whenIconsChanged: aBlock [ 
	
	iconHolder whenChangedDo: aBlock 


]
