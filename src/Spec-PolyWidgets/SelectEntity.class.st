"
A to-1 relationship field with keyboard selection with entry completion and a separate dialog for detailed searching (and creating new entities).

See my class side for an example


"
Class {
	#name : #SelectEntity,
	#superclass : #ComposablePresenter,
	#instVars : [
		'entity',
		'selectEntity',
		'possibleEntities',
		'displaySymbol',
		'entityText',
		'ghostText',
		'selectDialog'
	],
	#category : #'Spec-PolyWidgets-Widgets'
}

{ #category : #specs }
SelectEntity class >> defaultSpec [
	<spec: #default>
		
	^SpecLayout composed
		newColumn: [ :col |
				col newRow: [:row |
					row add: #entityText;
						add: #selectEntity width: ComposablePresenter toolbarHeight]
					height: ComposablePresenter toolbarHeight];
		yourself

]

{ #category : #example }
SelectEntity class >> example [
	self new
		selectDialog: [ FastTablePresenter new items: #(Henrik Peter); yourself];
		displaySymbol: #asString;
		ghostText: 'Names';
		possibleEntities: [#(Diego Stephan Norbert Stephane Marcus Torsten Sven Sean) sorted];
		openDialogWithSpec 

]

{ #category : #api }
SelectEntity >> displaySymbol [
	"<api: #inspect>"
	"return the method selector used to get the label of the entity that is for entry completion"
	^ displaySymbol value ifNil: [ #printString ]
]

{ #category : #api }
SelectEntity >> displaySymbol: anObject [
	displaySymbol value: anObject.
	entityText entryCompletion: self entitiesEntryCompletion.
]

{ #category : #entryCompletion }
SelectEntity >> entitiesEntryCompletion [
	^ EntryCompletion new
				dataSourceBlock: [:currText | self possibleEntities collect: [:each | 
		each perform: self displaySymbol ]];
				chooseBlock: [ :selectedName | 
					self entity: (self possibleEntities detect: [ :each | 
						(each perform: self displaySymbol) = selectedName ])];
				filterBlock: [:currEntity :currText | 
					currEntity asUppercase includesSubstring: currText asString asUppercase].
]

{ #category : #api }
SelectEntity >> entity [
	
	^ entity value
]

{ #category : #api }
SelectEntity >> entity: anObject [
	entity value: anObject.
	anObject ifNotNil: [  
		self entityText text: (anObject perform: self displaySymbol)] ifNil: [ self entityText text: '' ]
]

{ #category : #accessing }
SelectEntity >> entityText [
	^ entityText
]

{ #category : #accessing }
SelectEntity >> entityText: anObject [
	entityText := anObject
]

{ #category : #api }
SelectEntity >> ghostText [
	^ ghostText value ifNil: [ '' ]
]

{ #category : #api }
SelectEntity >> ghostText: aString [
	ghostText value: aString.
	entityText ghostText: aString
]

{ #category : #initialization }
SelectEntity >> initialize [
	super initialize.
	self initializeValueHolders
]

{ #category : #initialization }
SelectEntity >> initializePresenter [
	selectEntity 
		action: [  |select|
			select := self selectDialog value.
			select openDialogWithSpec
				okAction: [  
					self entity: select selectedItem ]]

]

{ #category : #initialization }
SelectEntity >> initializeValueHolders [
	displaySymbol := #displayString asValueHolder.
	entity := nil asValueHolder.
	ghostText := 'Choose one' asValueHolder.
	possibleEntities := #() asValueHolder.
	selectDialog := [  ] asValueHolder
]

{ #category : #initialization }
SelectEntity >> initializeWidgets [
	entityText := self newTextInput
		ghostText: self ghostText;
		autoAccept: true;
		entryCompletion: self entitiesEntryCompletion;
		yourself.
	selectEntity := self newButton
		icon: (self iconNamed: #glamorousSearch);
		help: 'Open a selection dialog'.
	self focusOrder
		add: entityText;
		add: selectEntity
]

{ #category : #api }
SelectEntity >> possibleEntities [
	possibleEntities value ifNil: [ ^#() ]
		ifNotNil: [ ^possibleEntities value value ]
]

{ #category : #api }
SelectEntity >> possibleEntities: aBlock [
	"<api: #block getter: #possibleEntities registration: #whenPossibleEntitiesChanged:>"
	"Set the block used to return a collection of all possible entity choices"
	possibleEntities value: aBlock.
	entityText entryCompletion: self entitiesEntryCompletion.
]

{ #category : #api }
SelectEntity >> selectDialog [
	^ selectDialog value
]

{ #category : #api }
SelectEntity >> selectDialog: anObject [
	"<api: #block getter: #selectDialog registration: #whenSelectDialogChanged:>"
	"Set a block that returns an object that will 
	first be sent #openDialogWithSpec
	and then as okAction sent #selectedItem"
	selectDialog value: anObject
]

{ #category : #accessing }
SelectEntity >> selectEntity [
	^ selectEntity
]

{ #category : #accessing }
SelectEntity >> selectEntity: anObject [
	selectEntity := anObject
]

{ #category : #'api-events' }
SelectEntity >> whenDisplaySymbolChanged: aBlock [
	displaySymbol whenChangedDo: aBlock
]

{ #category : #'api-events' }
SelectEntity >> whenEntityChanged: aBlock [
	entity whenChangedDo: aBlock
]

{ #category : #'api-events' }
SelectEntity >> whenGhostTextChanged: aBlock [
	ghostText whenChangedDo: aBlock
]

{ #category : #'api-events' }
SelectEntity >> whenPossibleEntitiesChanged: aBlock [
	possibleEntities whenChangedDo: aBlock
]

{ #category : #'api-events' }
SelectEntity >> whenSelectDialogChanged: aBlock [
	selectDialog whenChangedDo: aBlock
]
