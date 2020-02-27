"
An FastTablePresenter is a spec presenter for a FastTable
"
Class {
	#name : #FastTablePresenter,
	#superclass : #ListPresenter,
	#instVars : [
		'iconHolder',
		'displayBlock',
		'handlesDoubleClick',
		'doubleClick',
		'columns'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
FastTablePresenter class >> adapterName [

	^ #FastTableAdapter
]

{ #category : #specs }
FastTablePresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(FastTableAdapter
		adapt: #(model))
]

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
		whenSelectionChanged: [ :selection | selection traceCr ];
		openWithSpec.
	^ example
]

{ #category : #accessing }
FastTablePresenter >> columns [
	^ columns
]

{ #category : #api }
FastTablePresenter >> doubleClick: event [

	doubleClick value cull: event
]

{ #category : #api }
FastTablePresenter >> doubleClickAction [
	"<api: #inspect>"
	"Return the block evaluated on double click"
	
	^ doubleClick value
]

{ #category : #api }
FastTablePresenter >> doubleClickAction: aBlock [
	"<api: #block getter:#doubleClickAction registration: #whenDoubleClickActionChanged:>"
	"Set the block evaluated on double click."
	
	doubleClick value: aBlock
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
	super initialize.
	columns := {SpecFTColumn new}
]

{ #category : #initialization }
FastTablePresenter >> initializeValueHolders [

	super initializeValueHolders.

	iconHolder := [:item | nil ] asValueHolder.
	handlesDoubleClick := false asValueHolder.
	doubleClick := [  ] asValueHolder.

]

{ #category : #accessing }
FastTablePresenter >> numberOfColumns: aNumber [
	columns := ((1 to: aNumber) collect: [ :index | FTArrayIndexColumn index: index ] )
]

{ #category : #accessing }
FastTablePresenter >> numberOfColumns: aNumber withWidths: widths [
	columns := ((1 to: aNumber) collect: [ :index | 
		FTArrayIndexColumn index: index width: (widths at: index) ] )
]

{ #category : #private }
FastTablePresenter >> prepareForFilteredDataSourceWith: items [

	listHolder := items asValueHolder.
	listHolder whenChangedDo: [ self refreshListItems ].
	listItemsCache := nil
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
