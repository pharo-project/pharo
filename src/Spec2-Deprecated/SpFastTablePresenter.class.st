"
An FastTablePresenter is a spec presenter for a FastTable
"
Class {
	#name : #SpFastTablePresenter,
	#superclass : #SpTablePresenter,
	#instVars : [
		'#handlesDoubleClick => SpObservableSlot',
		'#displayBlock',
		'#icon => SpObservableSlot'
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
		whenSelectionChangedDo: [ :selection | selection traceCr ];
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
	^ self icons cull: anItem cull: self
]

{ #category : #api }
SpFastTablePresenter >> handlesDoubleClick [
	^ handlesDoubleClick
]

{ #category : #api }
SpFastTablePresenter >> handlesDoubleClick: aBoolean [
	"Enable or disable double click on the list."

	handlesDoubleClick := aBoolean
]

{ #category : #api }
SpFastTablePresenter >> icons [
	"Return the block which takes an item as argument and returns the icon to display in the list"

	^ icon
]

{ #category : #api }
SpFastTablePresenter >> icons: aBlock [
	"Set a block which takes an item as argument and returns the icon to display in the list"

	icon := aBlock
]

{ #category : #initialization }
SpFastTablePresenter >> initialize [
	super initialize.
	displayBlock := #yourself.
	icon := [ :item | nil ].
	handlesDoubleClick := false
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
	self property: #handlesDoubleClick whenChangedDo: [ :bool | self changed: { #handlesDoubleClick . bool } ].

]

{ #category : #'as yet unclassified' }
SpFastTablePresenter >> whenIconsChanged: aBlock [
	self deprecated: 'Use #whenIconsChangedDo: instead.' transformWith: '`@receiver whenIconsChanged: `@statements' -> '`@receiver whenIconsChangedDo: `@statements'.
	self whenIconsChangedDo: aBlock
]

{ #category : #'api-events' }
SpFastTablePresenter >> whenIconsChangedDo: aBlock [
	self property: #icon whenChangedDo: aBlock
]
