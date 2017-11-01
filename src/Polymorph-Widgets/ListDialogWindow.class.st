"
A ListDialogWindow is a dialog window used to search an element into a list.
A text field is provided to on the fly reduce the field or search
"
Class {
	#name : #ListDialogWindow,
	#superclass : #MessageDialogWindow,
	#instVars : [
		'pattern',
		'list',
		'searchMorph',
		'listMorph',
		'listIndex',
		'answer',
		'listBlock',
		'listCreationProcess',
		'displayBlock',
		'browseBlock',
		'acceptNewEntry'
	],
	#classInstVars : [
		'searchList'
	],
	#category : 'Polymorph-Widgets-Windows'
}

{ #category : #'instance creation' }
ListDialogWindow class >> chooseFromOwner: aMorph [

	^ self new chooseFromOwner: aMorph
]

{ #category : #accessing }
ListDialogWindow class >> searchList [
	^ searchList ifNil: [ searchList := OrderedCollection new ]
]

{ #category : #protocol }
ListDialogWindow >> accept: anItem [
	self answer: anItem.
	self ok.
]

{ #category : #accessing }
ListDialogWindow >> acceptNewEntry [

	^ acceptNewEntry
]

{ #category : #accessing }
ListDialogWindow >> acceptNewEntry: aBoolean [

	acceptNewEntry := aBoolean
]

{ #category : #accessing }
ListDialogWindow >> answer [
	^ answer
]

{ #category : #accessing }
ListDialogWindow >> answer: anObject [
	answer := anObject
]

{ #category : #'button behavior' }
ListDialogWindow >> browseAction [
	| aString tmp block |
	aString := searchMorph content.
	list detect: [ :item | (self displayItem: item) = aString ] ifFound: [ :item | self accept: item ].
	tmp := answer.
	block := self browseBlock.
	[ block value: tmp ] fork.
	self cancel
]

{ #category : #'button behavior' }
ListDialogWindow >> browseBlock [

	^ browseBlock
]

{ #category : #'button behavior' }
ListDialogWindow >> browseBlock: aBlock [

	browseBlock := aBlock
]

{ #category : #'item creation' }
ListDialogWindow >> buildBrowseButton [

	^ (PluggableButtonMorph
		on: self
		getState: #state
		action: #browseAction)
		label: 'Browse';
		yourself
]

{ #category : #'items creation' }
ListDialogWindow >> buildListMorph [
	^ listMorph := PluggableListMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		on: self
			list: #list
			selected: #listIndex
			changeSelected: #listIndex:
			menu: nil
			keystroke: nil;
		keystrokeSelector: #listKeystroke:;
		doubleClickSelector: #doubleClickOk;
		wrapSelector: #displayItem:;
		yourself
]

{ #category : #'items creation' }
ListDialogWindow >> buildSearchMorph [
	^ searchMorph := SearchMorph new
			model: self;
			acceptSelector: #searchAccept:;
			updateSelector: #searchUpdate:;
			searchList: self class searchList;
			keystrokeSelector: #searchKeystroke:
			yourself.	
	
]

{ #category : #actions }
ListDialogWindow >> cancel [

	self answer: nil.
	super cancel
]

{ #category : #'instance creation' }
ListDialogWindow >> chooseFromOwner: aMorph [

	aMorph openModal: self.
	^ self answer
]

{ #category : #'focus handling' }
ListDialogWindow >> defaultFocusMorph [

	^ searchMorph
]

{ #category : #display }
ListDialogWindow >> displayBlock [

	^ displayBlock
]

{ #category : #display }
ListDialogWindow >> displayBlock: aBlock [

	displayBlock := aBlock
]

{ #category : #display }
ListDialogWindow >> displayItem: anItem [

	^ self displayBlock cull: anItem cull: self
]

{ #category : #actions }
ListDialogWindow >> doubleClickOk [
	self listIndex ~= 0
		ifTrue: [ self ok ]
]

{ #category : #protocol }
ListDialogWindow >> getList: aSymbolOrBlockWithOneArgument [
	aSymbolOrBlockWithOneArgument isBlock
		ifTrue: [ listBlock := aSymbolOrBlockWithOneArgument. ^ self updateList ].
	aSymbolOrBlockWithOneArgument isSymbol 
		ifTrue: [ 
			listBlock := [ :regex| model perform: aSymbolOrBlockWithOneArgument with: regex].
			^ self updateList].
	Error signal: 'invalid argument'.
]

{ #category : #'focus handling' }
ListDialogWindow >> giveFocusToList [
	list ifEmpty: [ ^ self giveFocusToSearch].
	
	self listIndex: (( listIndex max: 1 ) min: list size).
	listMorph takeKeyboardFocus.
]

{ #category : #'focus handling' }
ListDialogWindow >> giveFocusToSearch [
	searchMorph takeKeyboardFocus.
]

{ #category : #open/close }
ListDialogWindow >> initialAnswer: aString [

	searchMorph content: aString.
]

{ #category : #open/close }
ListDialogWindow >> initialExtent [
	^ 300 @ 400
]

{ #category : #protocol }
ListDialogWindow >> initialFilter: aString [
	searchMorph content: aString.
]

{ #category : #initialization }
ListDialogWindow >> initialize [

	list := #().
	listIndex := 0.
	isResizeable := true.
	listBlock := [ :regex| #() ].
	displayBlock := [:e | e printString ].
	browseBlock :=  [:tmp | tmp browse ].
	pattern := '.' asRegexIgnoringCase.
	acceptNewEntry := false.
	
	super initialize.

]

{ #category : #'morphic protocol' }
ListDialogWindow >> list [
	^ list
]

{ #category : #'row management' }
ListDialogWindow >> listChanged [
	self changed: #list.
]

{ #category : #'morphic protocol' }
ListDialogWindow >> listIndex [
	^ listIndex 
]

{ #category : #'morphic protocol' }
ListDialogWindow >> listIndex: aNumber [
	listIndex := aNumber.
	self answer: (list at: listIndex ifAbsent: [ nil ]).
	self changed: #listIndex.
]

{ #category : #events }
ListDialogWindow >> listKeystroke: event [
	event keyCharacter = Character arrowUp 
		ifTrue: [ ^ self listKeystrokeUp ].
		
	event keyCharacter = Character arrowDown 
		ifTrue: [ ^ self listKeystrokeDown ].
]

{ #category : #events }
ListDialogWindow >> listKeystrokeDown [
	listIndex = list size
		ifTrue: [ 
			self listIndex: 0.
			self giveFocusToSearch.
			^ true].
	^ false.
]

{ #category : #events }
ListDialogWindow >> listKeystrokeUp [
	listIndex = 1 
		ifTrue: [ 
			self listIndex: 0.
			self giveFocusToSearch.
			^ true].
	^ false.
]

{ #category : #actions }
ListDialogWindow >> newButtons [
	"Answer new buttons as appropriate."

	^{self newOKButton isDefault: true. self buildBrowseButton. self newCancelButton}
]

{ #category : #actions }
ListDialogWindow >> newContentMorph [
	| panel  |
	panel := PanelMorph new.
	panel layoutPolicy: ProportionalLayout new;
		layoutInset: 0;
		hResizing: #spaceFill;
		vResizing: #spaceFill.					
	panel addMorph: self buildListMorph
			fullFrame: (LayoutFrame identity bottomOffset: -33).
	panel addMorph: self buildSearchMorph
			fullFrame: ((0@1 corner: 1@1) asLayoutFrame topOffset: -30).
	^ panel
]

{ #category : #private }
ListDialogWindow >> no [
	"overwrite de default"
]

{ #category : #actions }
ListDialogWindow >> ok [

	(acceptNewEntry and: [ list isEmpty ]) ifTrue: [ 
		self answer: self searchString ].
	
	self answer ifNil: [ self searchAccept: self searchString ].
	
	self 
		cancelled: false;
		delete
]

{ #category : #'morphic protocol' }
ListDialogWindow >> searchAccept: aString [
	self searchUpdate: aString.
	list detect: [ :item | (self displayItem: item) = aString ] ifFound: [ :item | ^ self accept: item ].
	acceptNewEntry
		ifTrue: [ ^ self accept: aString ]
		ifFalse: [ 
			list size = 1
				ifTrue: [ ^ self accept: list first ] ].
	list ifNotEmpty: [ ^ self giveFocusToList ]
]

{ #category : #events }
ListDialogWindow >> searchKeystroke: event [
	
	searchMorph content ifNil: [ ^ false ].
	
	(event keyCharacter = Character arrowDown )
		ifTrue: [ ^ self searchKeystrokeDown ].
		
	(event keyCharacter = Character arrowUp)
		ifTrue: [ ^ self searchKeystrokeUp ].
	
]

{ #category : #events }
ListDialogWindow >> searchKeystrokeDown [
	|interval|
	interval := searchMorph selectionInterval .
	((interval last == searchMorph content size)
	and: [ interval last < interval first ])
		ifFalse: [ ^ false ].
		
	self giveFocusToList.
	^ true
]

{ #category : #events }
ListDialogWindow >> searchKeystrokeUp [
	(searchMorph selectionInterval last == 0)
		ifFalse: [ ^ false ].
		
	self listIndex: list size.
	self giveFocusToList.
	^ true
]

{ #category : #accessing }
ListDialogWindow >> searchString [
	^ searchMorph searchString
]

{ #category : #'morphic protocol' }
ListDialogWindow >> searchUpdate: aString [

	pattern := '.' asRegexIgnoringCase.
	
	aString isEmptyOrNil ifFalse: [
		pattern := [aString asRegexIgnoringCase] on: RegexSyntaxError do: [ aString ]].
	
	self updateList.
]

{ #category : #'button behavior' }
ListDialogWindow >> state [

	^ false
]

{ #category : #'morphic protocol' }
ListDialogWindow >> updateList [
	"update the displayed list in a separate thread to avoid UI blocking"
	
	"if there is already a background thread running for the new list discard it"
	listCreationProcess ifNotNil: [
		listCreationProcess terminate].
	
	"no pattern given => empty list"
	pattern ifNil: [ ^ list :=#() ].
	
	"fork off a possibly costly list calculation"
	listCreationProcess := [
		list := listBlock value: pattern.
		"make sure the ui is updated in a synchronized manner"
		self defer: [self listChanged]] fork.
]

{ #category : #private }
ListDialogWindow >> yes [
	"overwrite de default"
]
