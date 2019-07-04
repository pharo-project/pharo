"
An FastTablePresenter is a spec presenter for a FastTable
"
Class {
	#name : #SpFastTablePresenter,
	#superclass : #SpTablePresenter,
	#instVars : [
		'iconHolder',
		'handlesDoubleClick',
		'displayBlock'
	],
	#category : #'Spec2-Deprecated-Replaced'
}

{ #category : #deprecation }
SpFastTablePresenter class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		icons: [ :e | example iconNamed: #forwardIcon ];
		items: (1 to: 10) asArray;
		openWithSpec.
	^ example
]

{ #category : #deprecation }
SpFastTablePresenter class >> exampleMultipleSelection [
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

{ #category : #deprecation }
SpFastTablePresenter class >> isDeprecated [
	^ true
]

{ #category : #initialization }
SpFastTablePresenter >> displayBlock [

	^ displayBlock
]

{ #category : #initialization }
SpFastTablePresenter >> displayBlock: aBlock [

	displayBlock := aBlock
]

{ #category : #api }
SpFastTablePresenter >> getIconFor: anItem [

	^ iconHolder value cull: anItem cull: self
]

{ #category : #api }
SpFastTablePresenter >> handlesDoubleClick [

	^ handlesDoubleClick value
]

{ #category : #api }
SpFastTablePresenter >> handlesDoubleClick: aBoolean [
	"<api: #block getter:#handlesDoubleClick registration: #whenHandlesDoubleClickChanged:>"
	"Enable or disable double click on the list."
	
	handlesDoubleClick value: aBoolean
]

{ #category : #api }
SpFastTablePresenter >> icons [
	"<api: #inspect>"
	"Return the block which takes an item as argument and returns the icon to display in the list"
	
	^ iconHolder value
]

{ #category : #api }
SpFastTablePresenter >> icons: aBlock [
	"<api: #block getter: #icon registration: #whenIconsChanged: >"
	"Set a block which takes an item as argument and returns the icon to display in the list"
	iconHolder value: aBlock
]

{ #category : #initialization }
SpFastTablePresenter >> initialize [

	displayBlock := #yourself.
	super initialize
]

{ #category : #initialization }
SpFastTablePresenter >> initializeValueHolders [

	super initializeValueHolders.

	iconHolder := [:item | nil ] asValueHolder.
	handlesDoubleClick := false asValueHolder.
]

{ #category : #'api-columns' }
SpFastTablePresenter >> numberOfColumns: aNumber [
	columns := OrderedCollection new.
	(1 to: aNumber) do: [ :index | self addColumn: (SpStringTableColumn evaluated: [ :entity | (self displayBlock value: entity) at: index ]) ]
]

{ #category : #'api-columns' }
SpFastTablePresenter >> numberOfColumns: aNumber withWidths: widths [
	columns := OrderedCollection new.
	(1 to: aNumber)
		do: [ :index | 
			self
				addColumn:
					((SpStringTableColumn evaluated: [ :entity | (self displayBlock value: entity) at: index ])
						width: (widths at: index);
						yourself) ]
]

{ #category : #initialization }
SpFastTablePresenter >> registerEvents [
	super registerEvents.
	handlesDoubleClick whenChangedDo: [ :bool | self changed: { #handlesDoubleClick . bool } ].

]

{ #category : #'as yet unclassified' }
SpFastTablePresenter >> whenIconsChanged: aBlock [
	self deprecated: 'Use #whenIconsChangedDo: instead.' transformWith: '`@receiver whenIconsChanged: `@statements' -> '`@receiver whenIconsChangedDo: `@statements'.
	self whenIconsChangedDo: aBlock
]

{ #category : #'api-events' }
SpFastTablePresenter >> whenIconsChangedDo: aBlock [ 
	
	iconHolder whenChangedDo: aBlock 


]
