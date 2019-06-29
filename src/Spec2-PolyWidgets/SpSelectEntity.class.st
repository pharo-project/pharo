"
A to-1 relationship field with keyboard selection with entry completion and a separate dialog for detailed searching (and creating new entities).

See my class side for an example


"
Class {
	#name : #SpSelectEntity,
	#superclass : #SpPresenter,
	#instVars : [
		'entity',
		'selectEntity',
		'possibleEntities',
		'displaySymbol',
		'entityText',
		'selectDialog',
		'placeholder'
	],
	#category : #'Spec2-PolyWidgets-Widgets'
}

{ #category : #specs }
SpSelectEntity class >> defaultSpec [
	<spec: #default>
		
	^SpLayout composed
		newColumn: [ :col |
				col newRow: [:row |
					row add: #entityText;
						add: #selectEntity width: SpPresenter toolbarHeight]
					height: SpPresenter toolbarHeight];
		yourself

]

{ #category : #api }
SpSelectEntity >> displaySymbol [
	"<api: #inspect>"
	"return the method selector used to get the label of the entity that is for entry completion"
	^ displaySymbol value ifNil: [ #printString ]
]

{ #category : #api }
SpSelectEntity >> displaySymbol: anObject [
	displaySymbol value: anObject.
	entityText entryCompletion: self entitiesEntryCompletion.
]

{ #category : #entryCompletion }
SpSelectEntity >> entitiesEntryCompletion [
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
SpSelectEntity >> entity [
	
	^ entity value
]

{ #category : #api }
SpSelectEntity >> entity: anObject [
	entity value: anObject.
	anObject ifNotNil: [  
		self entityText text: (anObject perform: self displaySymbol)] ifNil: [ self entityText text: '' ]
]

{ #category : #accessing }
SpSelectEntity >> entityText [
	^ entityText
]

{ #category : #accessing }
SpSelectEntity >> entityText: anObject [
	entityText := anObject
]

{ #category : #initialization }
SpSelectEntity >> initialize [
	super initialize.
	self initializeValueHolders
]

{ #category : #initialization }
SpSelectEntity >> initializePresenter [
	selectEntity 
		action: [  |select|
			select := self selectDialog value.
			select openDialogWithSpec
				okAction: [  
					self entity: select selectedItem ]]

]

{ #category : #initialization }
SpSelectEntity >> initializeValueHolders [
	displaySymbol := #displayString asValueHolder.
	entity := nil asValueHolder.
	placeholder := 'Choose one' asValueHolder.
	possibleEntities := #() asValueHolder.
	selectDialog := [  ] asValueHolder
]

{ #category : #initialization }
SpSelectEntity >> initializeWidgets [
	entityText := self newTextInput
		placeholder: self placeholder;
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
SpSelectEntity >> placeholder [
	^ placeholder value ifNil: [ '' ]
]

{ #category : #api }
SpSelectEntity >> placeholder: aString [
	placeholder value: aString.
	entityText placeholder: aString
]

{ #category : #api }
SpSelectEntity >> possibleEntities [
	possibleEntities value ifNil: [ ^#() ]
		ifNotNil: [ ^possibleEntities value value ]
]

{ #category : #api }
SpSelectEntity >> possibleEntities: aBlock [
	"<api: #block getter: #possibleEntities registration: #whenPossibleEntitiesChanged:>"
	"Set the block used to return a collection of all possible entity choices"
	possibleEntities value: aBlock.
	entityText entryCompletion: self entitiesEntryCompletion.
]

{ #category : #api }
SpSelectEntity >> selectDialog [
	^ selectDialog value
]

{ #category : #api }
SpSelectEntity >> selectDialog: anObject [
	"<api: #block getter: #selectDialog registration: #whenSelectDialogChanged:>"
	"Set a block that returns an object that will 
	first be sent #openDialogWithSpec
	and then as okAction sent #selectedItem"
	selectDialog value: anObject
]

{ #category : #accessing }
SpSelectEntity >> selectEntity [
	^ selectEntity
]

{ #category : #accessing }
SpSelectEntity >> selectEntity: anObject [
	selectEntity := anObject
]

{ #category : #'api-events' }
SpSelectEntity >> whenDisplaySymbolChanged: aBlock [
	displaySymbol whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSelectEntity >> whenEntityChanged: aBlock [
	entity whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSelectEntity >> whenPlaceholderChanged: aBlock [
	"aBlock will be performed when ghostText changed"
	
	placeholder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSelectEntity >> whenPossibleEntitiesChanged: aBlock [
	possibleEntities whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSelectEntity >> whenSelectDialogChanged: aBlock [
	selectDialog whenChangedDo: aBlock
]
