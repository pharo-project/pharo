"
MonticelloRepositoryBrowser example
"
Class {
	#name : #MonticelloRepositoryBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'repositories',
		'workingCopies',
		'order'
	],
	#classVars : [
		'Order'
	],
	#category : #'Spec-Tools-Monticello'
}

{ #category : #utils }
MonticelloRepositoryBrowser class >> allManagers [ 

	^ MCWorkingCopy allManagers 
]

{ #category : #specs }
MonticelloRepositoryBrowser class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		add: #(model workingCopies) right: 0.5;
		add: #(model repositories) left: 0.5;
		yourself.

]

{ #category : #example }
MonticelloRepositoryBrowser class >> example [
	<sampleInstance>
	"self example"
	
	self new 
		openWithSpec ; 
		workingCopies: (self allManagers)
]

{ #category : #utils }
MonticelloRepositoryBrowser class >> order [
	^ Order ifNil: [ Order := 1 ]
]

{ #category : #utils }
MonticelloRepositoryBrowser class >> order: anInteger [
	Order := anInteger
]

{ #category : #spec }
MonticelloRepositoryBrowser class >> title [

	^ 'Monticello Browser'
]

{ #category : #accessing }
MonticelloRepositoryBrowser >> allManagers [

	^ self class allManagers 
	
]

{ #category : #private }
MonticelloRepositoryBrowser >> allRepositories [

	^ MCRepositoryGroup default repositories
]

{ #category : #actions }
MonticelloRepositoryBrowser >> browseWorkingCopy [
	| workingCopy |
	workingCopy := self workingCopy.
	workingCopy
		ifNotNil: [ 
			(MCSnapshotBrowser forSnapshot: workingCopy completeSnapshot)
				label: 'Snapshot Browser: ' , workingCopy packageName;
				show ]
]

{ #category : #private }
MonticelloRepositoryBrowser >> defaultOrderingBlock [

	^ (self orderSpecs at: self class order) value
]

{ #category : #initialization }
MonticelloRepositoryBrowser >> initialExtent [

	^ 600@200
]

{ #category : #initialization }
MonticelloRepositoryBrowser >> initialize [

	super initialize.
	order := self class order
]

{ #category : #initialization }
MonticelloRepositoryBrowser >> initializePresenter [

	workingCopies whenSelectedItemChanged: 
			[ :item | repositories items: (self repositoriesOfWorkingCopy: item) ].
]

{ #category : #initialization }
MonticelloRepositoryBrowser >> initializeWidgets [

	repositories := self newList.
	workingCopies := self newList.
		
	workingCopies sortingBlock: self defaultOrderingBlock.
	workingCopies displayBlock: [ :item | item description ].
	workingCopies menu: [ :aMenu | self workingCopiesMenu: aMenu ].
		
	repositories displayBlock: [ :item | item description ].
	repositories items: self allRepositories.

	"so that when we tab we go from working copies to repositories"
	self focusOrder
		add: workingCopies;
		add: repositories
]

{ #category : #actions }
MonticelloRepositoryBrowser >> openRepository [
	self repository ifNotNil: [:repos | repos morphicOpen: self workingCopy ]
]

{ #category : #menu }
MonticelloRepositoryBrowser >> order [

	^ order
]

{ #category : #menu }
MonticelloRepositoryBrowser >> order: anInteger [

	order := anInteger.
	self workingCopies sortingBlock: (self orderSpecs at: order) value.
	self order = 4 "dirty only"
		ifTrue: [self workingCopies filteringBlock: [:col | col select: #modified]]
		ifFalse: [self workingCopies resetFilteringBlock]. 
]

{ #category : #menu }
MonticelloRepositoryBrowser >> orderSpecs [
	^ {
		'Sort alphabetically' -> [ :a :b | a package name <= b package name ].
		'Sort dirty first' -> [ :a :b | 
			a needsSaving = b needsSaving
				ifTrue: [ a package name <= b package name ]
				ifFalse: [ a needsSaving ] ].
		'Sort dirty last' -> [ :a :b | 
			a needsSaving = b needsSaving
				ifTrue: [ a package name <= b package name ]
				ifFalse: [ b needsSaving ] ].
		'Only dirty' -> [ :a :b | a package name <= b package name ]}
]

{ #category : #menu }
MonticelloRepositoryBrowser >> orderString: anIndex [
	^ String streamContents: [ :stream |
		order = anIndex
			ifTrue: [ stream nextPutAll: '<yes>' ]
			ifFalse: [ stream nextPutAll: '<no>' ].
		stream nextPutAll: (self orderSpecs at: anIndex) key ]
]

{ #category : #accessing }
MonticelloRepositoryBrowser >> repositories [

	^ repositories
]

{ #category : #private }
MonticelloRepositoryBrowser >> repositoriesOfWorkingCopy: aWorkingCopy [
	
	^ aWorkingCopy isNil
		ifFalse: [ aWorkingCopy repositoryGroup repositories]
		ifTrue: [ self allRepositories]	
]

{ #category : #accessing }
MonticelloRepositoryBrowser >> workingCopies [

	^ workingCopies
]

{ #category : #api }
MonticelloRepositoryBrowser >> workingCopies: aCollection [
	"Set the value of the list widget"
	
	workingCopies items: aCollection 
]

{ #category : #menu }
MonticelloRepositoryBrowser >> workingCopiesFromMC [
	^ (self orderSpecs size = order
		ifTrue: [ self allManagers select: [ :each | each modified ] ]
		ifFalse: [ self allManagers ])
			asSortedCollection: (self orderSpecs at: order) value.
]

{ #category : #menu }
MonticelloRepositoryBrowser >> workingCopiesMenu: aMenu [


	aMenu target: self;
		add: 'Browse working copy' selector: #browseWorkingCopy.

	1 to: self orderSpecs size do: [ :index | 
		aMenu
			addUpdating: #orderString:
			target: self
			selector: #order:
			argumentList: {index} ].
	^ aMenu
]

{ #category : #accessing }
MonticelloRepositoryBrowser >> workingCopy [
	^ workingCopies selectedItem
]
