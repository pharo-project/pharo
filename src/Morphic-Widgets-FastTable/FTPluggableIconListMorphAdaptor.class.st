"
I'm an adaptor to adapt a PluggableIconListMorph into a FastTable. 
I implement all required methods, but transform them to serve a table through a FTDataSource.
"
Class {
	#name : #FTPluggableIconListMorphAdaptor,
	#superclass : #FTTableMorph,
	#instVars : [
		'setSelectionListSelector',
		'getSelectionListSelector',
		'setIndexSelector',
		'getIndexSelector',
		'keystrokeSelector',
		'resetListSelector',
		'doubleClickSelector',
		'keystrokeActionSelector'
	],
	#category : #'Morphic-Widgets-FastTable-PluggableAdaptor'
}

{ #category : #'instance creation' }
FTPluggableIconListMorphAdaptor class >> on: anObject list: listSel primarySelection: getSelectionSel changePrimarySelection: setSelectionSel listSelection: getListSel changeListSelection: setListSel menu: getMenuSel [
	^ self new
		on: anObject
		list: listSel
		primarySelection: getSelectionSel
		changePrimarySelection: setSelectionSel
		listSelection: getListSel
		changeListSelection: setListSel
		menu: getMenuSel
		keystroke: #arrowKey:from:		"default"
]

{ #category : #'instance creation' }
FTPluggableIconListMorphAdaptor class >> on: anObject list: listSel primarySelection: getSelectionSel changePrimarySelection: setSelectionSel listSelection: getListSel changeListSelection: setListSel menu: getMenuSel keystroke: keyActionSel [ 
	^ self new
		on: anObject
		list: listSel
		primarySelection: getSelectionSel
		changePrimarySelection: setSelectionSel
		listSelection: getListSel
		changeListSelection: setListSel
		menu: getMenuSel
		keystroke: keyActionSel
]

{ #category : #'instance creation' }
FTPluggableIconListMorphAdaptor class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: nil
		keystroke: #arrowKey:from:		"default"
]

{ #category : #'instance creation' }
FTPluggableIconListMorphAdaptor class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: #arrowKey:from:		"default"

]

{ #category : #'instance creation' }
FTPluggableIconListMorphAdaptor class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: keyActionSel

]

{ #category : #'instance creation' }
FTPluggableIconListMorphAdaptor class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel wrapSelector: wrapSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: keyActionSel
		wrapSelector: wrapSel
]

{ #category : #'instance creation' }
FTPluggableIconListMorphAdaptor class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel wrapSelector: wrapSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: #arrowKey:from:	"default"
		wrapSelector: wrapSel
]

{ #category : #'drag and drop' }
FTPluggableIconListMorphAdaptor >> acceptDroppingMorph: aMorph event: event [
	"I need to more or less copy this from old pluggable list because there 
	 is much bad code around :("

	self dataSource dropItemSelector 
		ifNotNil: [ ^ super acceptDroppingMorph: aMorph event: event ].

	self model
		acceptDroppingMorph: aMorph
		event: event
		inMorph: self.
	self refresh
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptor >> autoDeselect [
	^ self resetListSelector 
		ifNotNil: [ true ]
		ifNil: [ self allowsDeselection ]
		
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptor >> autoDeselect: aBoolean [
	self allowsDeselection: aBoolean
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> basicGetListElementSelector: aSelector [
	self getListElementSelector: aSelector
]

{ #category : #updating }
FTPluggableIconListMorphAdaptor >> basicUpdateSelectionIndex [
	| rowIndex |
	self getIndexSelector ifNil: [ ^ self ].
	
	rowIndex := self model perform: self getIndexSelector.
	rowIndex = self selectedIndex ifTrue: [ ^ self ].
	rowIndex = 0
		ifTrue: [ self basicSelectIndexes: #() ]
		ifFalse: [ self basicSelectIndexes: { rowIndex } ]
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> basicWrapSelector: aSelector [
	self dataSource wrapSelector: aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> doubleClickSelector [
	^ doubleClickSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> doubleClickSelector: aSelector [
	doubleClickSelector := aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> dropItemSelector: aSelector [
	self dataSource dropItemSelector: aSelector

]

{ #category : #accessing }
FTPluggableIconListMorphAdaptor >> getCurrentSelectionIndex [
	"Answer the index of the current selection."
	self getIndexSelector ifNil: [ ^0 ].
	^self model perform: self getIndexSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getIconSelector: aSelector [
	self dataSource getIconSelector: aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getIndexSelector [
	^ getIndexSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getIndexSelector: aSelector [
	getIndexSelector := aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getListElementSelector [
	^ self dataSource getListElementSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getListElementSelector: aSelector [
	self dataSource getListElementSelector: aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getListSelector [
	^ self dataSource getListSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getListSelector: aSelector [
	self dataSource getListSelector: aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getListSizeSelector: aSelector [
	self dataSource getListSizeSelector: aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getMenuSelector: aSelector [
	self dataSource getMenuSelector: aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getSelectionListSelector [
	^ getSelectionListSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> getSelectionListSelector: aSelector [
	getSelectionListSelector := aSelector
]

{ #category : #initialization }
FTPluggableIconListMorphAdaptor >> initialize [
	super initialize.
	self initializeSelections.
	self hideColumnHeaders.
	self dataSource: FTPluggableIconListMorphAdaptorDataSource new.

]

{ #category : #initialization }
FTPluggableIconListMorphAdaptor >> initializeKeyBindings [ 
	super initializeKeyBindings.
	self 
		bindKeyCombination: Character space asKeyCombination
		toAction: [ self interactWithSelection ].
	
]

{ #category : #initialization }
FTPluggableIconListMorphAdaptor >> initializeSelections [
	self 
		onAnnouncement: FTSelectionChanged  
		send: #selectionChanged: 
		to: self.
	self 
		onAnnouncement: FTStrongSelectionChanged  
		send: #strongSelectionChanged: 
		to: self.	
]

{ #category : #private }
FTPluggableIconListMorphAdaptor >> interactWithSelection [
	| cell |
	
	self hasSelection ifFalse: [ ^ self ].
	"this is a horrible hack, but well... that's how it is implemented :("
	cell := (self container exposedRows at: self selectedIndex) submorphs first. "the cell is there"
	cell submorphsDo: [ :each | each update: #interact ]
]

{ #category : #'event handling' }
FTPluggableIconListMorphAdaptor >> keyStroke: event [
	(super keyStroke: event) ifTrue: [ ^ true ].
	self keystrokeSelector 
		ifNotNil: [ (self keyStrokeAction: event) ifTrue: [ ^ true ] ].
	(self keystrokeActionSelector notNil 
		and: [ event anyModifierKeyPressed ])
		ifTrue: [ (self keyStrokeModifierAction: event) ifTrue: [ ^ true ] ].
	
	^ false
]

{ #category : #'event handling' }
FTPluggableIconListMorphAdaptor >> keyStrokeAction: event [
	| result |
	
	result := self model
		perform: self keystrokeSelector
		withEnoughArguments: { event. self }.
		
	^ result = true
]

{ #category : #'event handling' }
FTPluggableIconListMorphAdaptor >> keyStrokeModifierAction: event [
	| args character |
	character := event keyCharacter.
	args := self keystrokeActionSelector numArgs.
	args = 1 ifTrue: [ 
		^ self model perform: self keystrokeActionSelector with: character].
	args = 2 ifTrue: [
		^ self model 
				perform: self keystrokeActionSelector
				with: character
				with: self ].
			
	^self error: 'keystrokeActionSelector must be a 1- or 2-keyword symbol'		

]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> keystrokeActionSelector [
	^ keystrokeActionSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> keystrokeActionSelector: aSelector [
	keystrokeActionSelector := aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> keystrokeSelector [
	^ keystrokeSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> keystrokeSelector: aSelector [
	keystrokeSelector := aSelector
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptor >> model [
	^ self dataSource model
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptor >> model: anObject [ 
	| oldModel |
	oldModel := self dataSource model. 
	oldModel ifNotNil: [ oldModel removeDependent: self ].
	anObject ifNotNil: [ anObject addDependent: self ].
	self dataSource model: anObject
]

{ #category : #initialization }
FTPluggableIconListMorphAdaptor >> on: anObject list: listSel primarySelection: getSelectionSel changePrimarySelection: setSelectionSel listSelection: getListSel changeListSelection: setListSel menu: getMenuSel keystroke: keyActionSel [
	"setup a whole load of pluggability options"
	
	self getSelectionListSelector: getListSel.
	self setSelectionListSelector: setListSel.
	self 
		on: anObject 
		list: listSel 
		selected: getSelectionSel 
		changeSelected: setSelectionSel 
		menu: getMenuSel 
		keystroke: keyActionSel.
	self beMultipleSelection
]

{ #category : #initialization }
FTPluggableIconListMorphAdaptor >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel [ 
	self model: anObject.
	self getListSelector: getListSel.
	self getIndexSelector: getSelectionSel.
	self setIndexSelector: setSelectionSel.
	self getMenuSelector: getMenuSel.
	self keystrokeActionSelector: keyActionSel
]

{ #category : #initialization }
FTPluggableIconListMorphAdaptor >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel wrapSelector: wrapSel [
	self model: anObject.
	self getListSelector: getListSel.
	self getIndexSelector: getSelectionSel.
	self setIndexSelector: setSelectionSel.
	self getMenuSelector: getMenuSel.
	self keystrokeActionSelector: keyActionSel.
	self basicWrapSelector: wrapSel

]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> resetListSelector [
	^ resetListSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> resetListSelector: aSelector [
	resetListSelector := aSelector
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptor >> scrollValue [
	"x is always zero because horizontal scroll bar is not yet implemented"

	^ 0 @ self verticalScrollBar value
]

{ #category : #ignored }
FTPluggableIconListMorphAdaptor >> searchedElement: index [
	self highlightIndex: index
]

{ #category : #private }
FTPluggableIconListMorphAdaptor >> selectionChanged: ann [
	self selectionChangedUpdateStatus: ann.
	self selectionChangedUpdateIndex: ann.
	self selectionChangedResetIfEmpty: ann
]

{ #category : #private }
FTPluggableIconListMorphAdaptor >> selectionChangedResetIfEmpty: ann [
	self resetListSelector ifNil: [ ^ self ].
	ann newSelectedIndexes ifNotEmpty: [ ^ self ].

	self model perform: self resetListSelector
]

{ #category : #private }
FTPluggableIconListMorphAdaptor >> selectionChangedUpdateIndex: ann [
	self setIndexSelector ifNil: [ ^ self ].

	self model perform: self setIndexSelector with: (ann newSelectedIndexes ifNotEmpty: [ :indexes | indexes first ] ifEmpty: [ 0 ])
]

{ #category : #private }
FTPluggableIconListMorphAdaptor >> selectionChangedUpdateStatus: ann [
	"Updates the status of selection (informs new selections and deselections)"

	self setSelectionListSelector ifNil: [ ^ self ].

	ann oldSelectedIndexes do: [ :each | self model perform: self setSelectionListSelector with: each with: false ].

	ann newSelectedIndexes do: [ :each | self model perform: self setSelectionListSelector with: each with: true ]
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> setIndexSelector [
	^ setIndexSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> setIndexSelector: aSelector [
	setIndexSelector := aSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> setSelectionListSelector [
	^ setSelectionListSelector
]

{ #category : #'accessing selectors' }
FTPluggableIconListMorphAdaptor >> setSelectionListSelector: aSelector [
	setSelectionListSelector := aSelector
]

{ #category : #private }
FTPluggableIconListMorphAdaptor >> strongSelectionChanged: ann [
	self doubleClickSelector ifNil: [ ^ self ].
	
	self model perform: self doubleClickSelector withEnoughArguments: { ann }
]

{ #category : #updating }
FTPluggableIconListMorphAdaptor >> update: symbol [
	(symbol == self getListSelector or: [ symbol == self getListElementSelector ]) 
		ifTrue: [ ^ self updateList ].
		
	symbol == self getIndexSelector
		ifTrue: [ ^ self updateSelectionIndex ].
]

{ #category : #updating }
FTPluggableIconListMorphAdaptor >> updateList [
	(showIndex < self dataSource numberOfRows and: [ self isIndexVisible: showIndex ])
		ifFalse: [ self resetPosition ].
	self basicUpdateSelectionIndex.
	(self hasSelection and: [ (self isIndexVisible: self selectedIndex) ])
		ifFalse: [ self resetPosition.
			self ensureVisibleFirstSelection ].
	self refresh
]

{ #category : #updating }
FTPluggableIconListMorphAdaptor >> updateSelectionIndex [
	self basicUpdateSelectionIndex.
	self refresh
]

{ #category : #accessing }
FTPluggableIconListMorphAdaptor >> vScrollValue: aNumber [
	self verticalScrollBar setValue: aNumber
]

{ #category : #'drag and drop' }
FTPluggableIconListMorphAdaptor >> wantsDroppedMorph: aMorph event: anEvent [
	"I need to more or less copy this from old pluggable list because there 
	 is much bad code around :("

	^ aMorph dragTransferType == #dragTransfer 
		ifTrue: [ super wantsDroppedMorph: aMorph event: anEvent ]
		ifFalse: [ self model wantsDroppedMorph: aMorph event: anEvent inMorph: self ]
]
