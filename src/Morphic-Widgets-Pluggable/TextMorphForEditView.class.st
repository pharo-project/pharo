"
I'm a TextMorph that must be embedded in a PluggableTextMorph.
Multi-line text editor with support for accepting on both each change and/or when keyboard focus changes. Also supports custom selection colour and clickable highlights From PluggableTextEditorMorph.

Instance Variables
	acceptOnCR:		<Object>
	acceptOnFocusChange:		<Object>
	autoAccept:		<Object>
	editView:		<Object>
	selectionColor:		<Object>

acceptOnCR
	- xxxxx

acceptOnFocusChange
	- xxxxx

autoAccept
	- xxxxx

editView
	- xxxxx

selectionColor
	- xxxxx

"
Class {
	#name : #TextMorphForEditView,
	#superclass : #TextMorph,
	#instVars : [
		'editView',
		'acceptOnCR',
		'autoAccept',
		'acceptOnFocusChange',
		'selectionColor'
	],
	#category : #'Morphic-Widgets-Pluggable-Text'
}

{ #category : #editing }
TextMorphForEditView >> acceptContents [
	"The message is sent when the user hits enter or Cmd-S.
	Accept the current contents and end editing."
	self updateFromParagraph.
	editView accept.
]

{ #category : #editing }
TextMorphForEditView >> acceptOnCR [
	"Answer whether the receiver wants to accept when the Return key is hit"
	"This test is to make sure that existing instances that may be changed and get their acceptOnCR 
	do not blow up on our face. Now this test could be removed"

	^ acceptOnCR == true
]

{ #category : #'accept/cancel' }
TextMorphForEditView >> acceptOnCR: trueOrFalse [
	acceptOnCR := trueOrFalse
]

{ #category : #'accept/cancel' }
TextMorphForEditView >> acceptOnFocusChange [
	"Answer the value of acceptOnFocusChange"

	^ acceptOnFocusChange
]

{ #category : #'accept/cancel' }
TextMorphForEditView >> acceptOnFocusChange: anObject [
	"Set the value of acceptOnFocusChange"

	acceptOnFocusChange := anObject
]

{ #category : #accessing }
TextMorphForEditView >> autoAccept [
	"Answer the value of autoAccept"

	^ autoAccept
]

{ #category : #accessing }
TextMorphForEditView >> autoAccept: anObject [
	"Set the value of autoAccept"

	autoAccept := anObject
]

{ #category : #'event handling' }
TextMorphForEditView >> autoScrollView: evt [
	"This is kind of a hack because the PluggableTextMorph expects me to first expand the selection before auto scrolling will work."
	| localEvt |
	localEvt := evt transformedBy: (self transformedFrom: editView).
	super mouseMove: localEvt.
	editView scrollSelectionIntoView: localEvt.
]

{ #category : #binding }
TextMorphForEditView >> bindingOf: varName [
	^ self editor bindingOf: varName
]

{ #category : #editing }
TextMorphForEditView >> canChangeText [
	^ self editView enabled
]

{ #category : #editing }
TextMorphForEditView >> cancelEdits [
	"The message is sent when the user hits enter or Cmd-L.
	Cancel the current contents and end editing."
	self releaseParagraph.
	editView cancel
]

{ #category : #'accept/cancel' }
TextMorphForEditView >> doAutoAccept: evt [
	"Do the key stroke and check to see if it should be accepted."

	self autoAccept == true 
		ifTrue: [self editView hasUnacceptedEdits 
			ifTrue: [self editor accept]]
]

{ #category : #'event handling' }
TextMorphForEditView >> doubleClickOnSelectedText: event [
	"Unselect everything. This message is sent by the editor"
	
	self deselect
]

{ #category : #'event handling' }
TextMorphForEditView >> dragEnabled [

	^ self editView dragEnabled
]

{ #category : #drawing }
TextMorphForEditView >> drawNullTextOn: aCanvas [
	"Just run the normal code to show selection in a window"
	aCanvas paragraph: self paragraph bounds: bounds color: color

]

{ #category : #drawing }
TextMorphForEditView >> drawOn: aCanvas [
	self useSelectionBar
		ifTrue: [self paragraph displaySelectionBarOn: aCanvas].
	self useExtraSelection 
		ifTrue: [self paragraph displayExtraSelectionOn: aCanvas].
	super drawOn: aCanvas.

]

{ #category : #'edit view' }
TextMorphForEditView >> editView [
	^ editView
]

{ #category : #'event handling' }
TextMorphForEditView >> escapePressed [
	^ self editView yellowButtonActivity: false. 
]

{ #category : #'find-replace' }
TextMorphForEditView >> findAndSelect: aRegex startingAt: anIndex searchBackwards: searchBackwards [
	| where |
	self
		handleEdit: [
			where := self editor
				findAndSelect: aRegex
				startingAt: anIndex
				searchBackwards: searchBackwards].
	editView scrollSelectionIntoView.
	^ where
]

{ #category : #'find-replace' }
TextMorphForEditView >> findNextString: aSubstring startingAt: searchIdx [
	| where |
	self
		handleEdit: [where := self editor findNextString: aSubstring startingAt: searchIdx].
	^ where

]

{ #category : #private }
TextMorphForEditView >> findRegex [
	^ self editor findRegex
]

{ #category : #'find-replace' }
TextMorphForEditView >> findReplaceDialog [
	^ EditorFindReplaceDialogWindow singleton
]

{ #category : #private }
TextMorphForEditView >> findText [
	^ self editor findText
]

{ #category : #macpal }
TextMorphForEditView >> flash [
	^ editView flash
]

{ #category : #editing }
TextMorphForEditView >> handleInteraction: interActionBlock [
	"Overridden to pass along a model to the editor for, eg, link resolution, doits, etc"

	self editor model: editView model.  "For evaluateSelection, etc"
	^ super handleInteraction: interActionBlock
]

{ #category : #'event handling' }
TextMorphForEditView >> handlesDoubleClick [

	^ self editView handlesDoubleClick
]

{ #category : #editing }
TextMorphForEditView >> hasUnacceptedEdits: aBoolean [
	"Set the hasUnacceptedEdits flag in my view."

	editView hasUnacceptedEdits: aBoolean
]

{ #category : #initialization }
TextMorphForEditView >> initialize [
	super initialize.
	acceptOnCR := false.
	autoAccept := false.
	acceptOnFocusChange := false
]

{ #category : #'event handling' }
TextMorphForEditView >> keyStroke: evt [
	"Handle a keystroke. Deal with navigation keys also."
	
	| view |  
	
	editView keyStroke: evt.
	(editView scrollByKeyboard: evt) ifTrue: [^self].
	(acceptOnCR and: [evt keyCharacter = Character cr])
		ifFalse: [(editView navigationKey: evt) ifTrue: [^self]].
	self editor model: editView model.  "For evaluateSelection"
	view := editView.  "Copy into temp for case of a self-mutating doit"
	(acceptOnCR and: [evt keyCharacter = Character cr])
		ifTrue: [^ self editor accept].
	super keyStroke: evt.
	view textChanged.
	view scrollSelectionIntoView.
	self doAutoAccept: evt.
	view scrollSelectionIntoView
]

{ #category : #changed }
TextMorphForEditView >> keyboardFocusChange: gotFocus [ 
	self announceKeyboardFocusChange: gotFocus.
 	paragraph 
		ifNotNil: [paragraph focused: gotFocus].
 	gotFocus 
 		ifTrue: [
			"A hand is wanting to send us characters..."
 			self hasFocus 
				ifFalse: [self editor "Forces editor install"].
			self editor focused: true.
			self showOverEditableTextCursor.
			self useFindReplaceSelection
				ifTrue: [self selectionChanged]] 
		ifFalse:[editor ifNotNil: [editor focused: false].
			self hideOverEditableTextCursor].
	self manageCursor.		
 	self editView
		ifNil: [self focusChanged]
		ifNotNil: [self editView focusChanged].
	self acceptOnFocusChange == true
		ifTrue: [self editView hasUnacceptedEdits ifTrue: [self editor accept]]
]

{ #category : #'event handling' }
TextMorphForEditView >> mouseDown: event [
	self editView ifNotNil: [ self editView mouseDownFromTextMorph: event ].
	super mouseDown: event.
	self flag: #hack.
	(self editView respondsTo: #highlights)
		ifTrue: [ 
			self editView highlights
				detect: [ :h | h containsPoint: event position in: (self bounds: self editView innerBounds from: self) ]
				ifFound: [ :hl | hl clicked: event ] ]
]

{ #category : #'event handling' }
TextMorphForEditView >> mouseMove: evt [
	| editEvt |
	super mouseMove: evt.
	evt redButtonPressed ifFalse: [^ self].
	editEvt := evt transformedBy: (self transformedFrom: editView) inverseTransformation.
	(editEvt position y between: editView top and: editView bottom) ifFalse:[
		"Start auto-scrolling"
		self startStepping: #autoScrollView:
			at: Time millisecondClockValue
			arguments: (Array with: editEvt)
			stepTime: 100. "fast enough"
	] ifTrue:[
		self stopSteppingSelector: #autoScrollView:.
	].
]

{ #category : #'event handling' }
TextMorphForEditView >> mouseUp: event [
	super mouseUp: event.
	
	self stopSteppingSelector: #autoScrollView:.
	"editView scrollSelectionIntoView: evt."
	editView selectionInterval: editor selectionInterval.
]

{ #category : #'find-replace' }
TextMorphForEditView >> openFindDialog [
	self sharesFindReplace 
		ifTrue: [ (self findReplaceDialog on: self) open]
		ifFalse: [self flash]
]

{ #category : #accessing }
TextMorphForEditView >> paragraph [
	"Answer the paragraph."

	^ super paragraph selectionColor: self selectionColor; yourself

]

{ #category : #'event handling' }
TextMorphForEditView >> preferredKeyboardPosition [

	| pos |
	pos := super preferredKeyboardPosition.
	^ pos + (self bounds: self bounds in: self currentWorld) topLeft.

]

{ #category : #'multi level undo' }
TextMorphForEditView >> redoTypeIn: aText interval: anInterval [ 
	self handleEdit: [self editor redoTypeIn: aText interval: anInterval]
]

{ #category : #private }
TextMorphForEditView >> refreshExtraSelection [
	| fullRefreshNeeded |
	fullRefreshNeeded := self useSelectionBar.
	self useFindReplaceSelection
		ifTrue: [ 
			fullRefreshNeeded := true.
			self findText isEmpty
				ifTrue: [ self paragraph findReplaceSelectionRegex: nil ]
				ifFalse: [ self paragraph findReplaceSelectionRegex: self findRegex ] ].
	self useSecondarySelection
		ifTrue: [ 
			fullRefreshNeeded := true.
			(self paragraph secondarySelection isNil and: [ self selection isEmptyOrNil not ])
				ifTrue: [ self paragraph secondarySelection: (self text asString allRangesOfSubstring: self selection asString) ] ].
	fullRefreshNeeded
		ifTrue: [ 
			self paragraph extraSelectionChanged.
			"> Alain: should be that : 
			 > self paragraph extraSelectionRects do: [:r | self invalidRect: r]
			 > but it doesn't work"
			self invalidRect: self bounds ]
]

{ #category : #'find-replace' }
TextMorphForEditView >> replaceAll: aRegex with: aText [
	self
		handleEdit: [self editor replaceAll: aRegex with: aText].

]

{ #category : #'find-replace' }
TextMorphForEditView >> replaceAll: aRegex with: aText startingAt: startIdx [
	self
		handleEdit: [self editor replaceAll: aRegex with: aText startingAt: startIdx].

]

{ #category : #'find-replace' }
TextMorphForEditView >> replaceSelectionWith: aReplacementText [
	self handleEdit: [self editor replaceSelectionWith: aReplacementText]
]

{ #category : #miscellaneous }
TextMorphForEditView >> selectAll [
	"Tell my editor to select all the text"

	self editor selectAll
]

{ #category : #private }
TextMorphForEditView >> selectionChanged [
	self sharesFindReplace 
		ifTrue: [self editor findReplaceService changed: #findReplaceSelection].
	self useExtraSelection
		ifTrue: [self refreshExtraSelection].
	super selectionChanged
]

{ #category : #accessing }
TextMorphForEditView >> selectionColor [
	^ selectionColor
]

{ #category : #accessing }
TextMorphForEditView >> selectionColor: aColor [
	selectionColor := aColor.
	paragraph ifNotNil: [:p | p selectionColor: aColor]
]

{ #category : #'edit view' }
TextMorphForEditView >> setEditView: editPane [
	editView := editPane
]

{ #category : #'find-replace' }
TextMorphForEditView >> sharesFindReplace [
	^ true
]

{ #category : #blinking }
TextMorphForEditView >> startBlinking [
	self canChangeText 
		ifTrue: [super startBlinking]
		ifFalse: [self paragraph showCaret: true]
]

{ #category : #dragging }
TextMorphForEditView >> startDrag: event [

	self editView startDrag: event
]

{ #category : #'multi level undo' }
TextMorphForEditView >> undoRedoExchange: aninterval with: anotherInterval [
	self handleEdit: [self editor undoRedoExchange: aninterval with: anotherInterval]
]

{ #category : #'multi level undo' }
TextMorphForEditView >> undoTypeIn: aText interval: anInterval [ 
	self handleEdit: [self editor undoTypeIn: aText interval: anInterval]
]

{ #category : #private }
TextMorphForEditView >> updateFromParagraph [  
	super updateFromParagraph.
	editView setScrollDeltas.
]

{ #category : #private }
TextMorphForEditView >> useExtraSelection [
	^ self useFindReplaceSelection or: [self useSecondarySelection]

]

{ #category : #private }
TextMorphForEditView >> useFindReplaceSelection [
	^ TextEditor useFindReplaceSelection
]

{ #category : #private }
TextMorphForEditView >> useSecondarySelection [
	^ TextEditor useSecondarySelection
]

{ #category : #private }
TextMorphForEditView >> useSelectionBar [
	^ TextEditor useSelectionBar
]

{ #category : #'event handling' }
TextMorphForEditView >> wouldAcceptKeyboardFocusUponTab [
	"Answer whether the receiver would be a happy inheritor of keyboard focus if tab were hit in an enclosing playfield under propitious circumstances.  Does not make sense for this kind of morph, which is encased in a window"

	^ false
]
