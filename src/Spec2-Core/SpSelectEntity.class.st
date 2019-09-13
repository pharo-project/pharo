"
A to-1 relationship field with keyboard selection with entry completion and a separate dialog for detailed searching (and creating new entities).

See my class side for an example


"
Class {
	#name : #SpSelectEntity,
	#superclass : #SpPresenter,
	#instVars : [
		'#entity => SpObservableSlot',
		'#selectEntity',
		'#possibleEntities => SpObservableSlot',
		'#displaySymbol',
		'#entityText',
		'#selectDialog => SpObservableSlot',
		'#placeholder => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpSelectEntity class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
				add: #entityText;
				add: #selectEntity withConstraints: [ :constraints | constraints width: self toolbarHeight ];
				yourself)
			withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		yourself
]

{ #category : #initialization }
SpSelectEntity >> connectPresenters [
	selectEntity 
		action: [  |select|
			select := self selectDialog value.
			select openDialogWithSpec
				okAction: [  
					self entity: select selectedItem ]]

]

{ #category : #api }
SpSelectEntity >> displaySymbol [
	"return the method selector used to get the label of the entity that is for entry completion"

	^ displaySymbol ifNil: [ #printString ]
]

{ #category : #api }
SpSelectEntity >> displaySymbol: anObject [
	displaySymbol := anObject.
	entityText entryCompletion: self entitiesEntryCompletion
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
	^ entity
]

{ #category : #api }
SpSelectEntity >> entity: anObject [
	entity := anObject.
	anObject ifNotNil: [ self entityText text: (anObject perform: self displaySymbol) ] ifNil: [ self entityText text: '' ]
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
	displaySymbol := #displayString.
	placeholder := 'Choose one'.
	possibleEntities := #().
	selectDialog := [  ]
]

{ #category : #initialization }
SpSelectEntity >> initializePresenters [
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
	^ placeholder ifNil: [ '' ]
]

{ #category : #api }
SpSelectEntity >> placeholder: aString [
	placeholder := aString.
	entityText placeholder: aString
]

{ #category : #api }
SpSelectEntity >> possibleEntities [
	^ possibleEntities ifNil: [ #() ] ifNotNil: [ possibleEntities value ]
]

{ #category : #api }
SpSelectEntity >> possibleEntities: aBlock [
	"Set the block used to return a collection of all possible entity choices"

	possibleEntities := aBlock.
	entityText entryCompletion: self entitiesEntryCompletion
]

{ #category : #api }
SpSelectEntity >> selectDialog [
	^ selectDialog
]

{ #category : #api }
SpSelectEntity >> selectDialog: anObject [
	"Set a block that returns an object that will 
	first be sent #openDialogWithSpec
	and then as okAction sent #selectedItem"

	selectDialog := anObject
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
	self property: #displaySymbol whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSelectEntity >> whenEntityChanged: aBlock [
	self property: #entity whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSelectEntity >> whenPlaceholderChanged: aBlock [
	"aBlock will be performed when ghostText changed"

	self property: #placeholder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSelectEntity >> whenPossibleEntitiesChanged: aBlock [
	self property: #possibleEntities whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpSelectEntity >> whenSelectDialogChanged: aBlock [
	self property: #selectDialog whenChangedDo: aBlock
]
