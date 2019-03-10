"
This class was introduced to ease the migration to Rubric.
Thus, I''am a replacement for compatibility with old PluggableTextMorph.
It is still partly based on change/update.
Do not use this class for new developments since it will be deprecated asap.
Use RubScrolledTextMorph instead

"
Class {
	#name : #RubPluggableTextMorph,
	#superclass : #RubScrolledTextMorph,
	#instVars : [
		'getEnabledSelector'
	],
	#category : #'Rubric-Compatibility-toBeDeprecated'
}

{ #category : #'instance creation' }
RubPluggableTextMorph class >> on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel [

	^ self new on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: getSelectionSel
		menu: getMenuSel
		setSelection: nil
]

{ #category : #'instance creation' }
RubPluggableTextMorph class >> on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel setSelection: setSelectionSel [

	^ self new on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: getSelectionSel
		menu: getMenuSel
		setSelection: setSelectionSel
]

{ #category : #'undo-redo' }
RubPluggableTextMorph >> clearUndoManager [
	self textArea editingState clearUndoManager: nil
]

{ #category : #scrolling }
RubPluggableTextMorph >> disable [
	self beReadOnly 
]

{ #category : #scrolling }
RubPluggableTextMorph >> enable [
	self beEditable 
]

{ #category : #accessing }
RubPluggableTextMorph >> getColorSelector [
	^ self getBackgroundColorSelector 
]

{ #category : #accessing }
RubPluggableTextMorph >> getColorSelector: aSymbol [
	self getBackgroundColorSelector: aSymbol.
	self update: self getColorSelector.
]

{ #category : #accessing }
RubPluggableTextMorph >> getEnabledSelector [
	"Answer the value of getEnabledSelector"

	^ getEnabledSelector
]

{ #category : #accessing }
RubPluggableTextMorph >> getEnabledSelector: aSymbol [
	"Answer the value of getEnabledSelector"

	getEnabledSelector := aSymbol
]

{ #category : #accessing }
RubPluggableTextMorph >> getMenuSelector: aGetMenuSelector [
	aGetMenuSelector
		ifNil: [ self textArea forbidMenu ]
		ifNotNil: [ self textArea allowMenu.
			self menuProvider: self model selector: aGetMenuSelector ]
]

{ #category : #accessing }
RubPluggableTextMorph >> hasUnacceptedEdits: aBoolean [
	"Set the hasUnacceptedEdits flag to the given value. "

	(self model respondsTo: #hasUnacceptedEdits:)
		ifTrue: [ self model hasUnacceptedEdits: aBoolean ]
		ifFalse: [ 
			super hasUnacceptedEdits: aBoolean.
			self changed ].
	aBoolean
		ifFalse: [ hasEditingConflicts := false ]
]

{ #category : #initialization }
RubPluggableTextMorph >> initialize [
	super initialize.
	hasUnacceptedEdits := false.
	hasEditingConflicts := false.
	askBeforeDiscardingEdits := true
]

{ #category : #'model protocol' }
RubPluggableTextMorph >> model: anObject [
	super model: anObject.
	anObject addDependent: self
]

{ #category : #'model protocol' }
RubPluggableTextMorph >> on: aModel text: aGetTextSelector accept: aSetTextSelector readSelection: aReadSelectionSelector menu: aGetMenuSelector [
	self getTextSelector: aGetTextSelector.
	self setTextSelector: aSetTextSelector.
	self getSelectionSelector: aReadSelectionSelector.
	aGetMenuSelector
		ifNil: [ self textArea forbidMenu ]
		ifNotNil: [ self menuProvider: aModel selector: aGetMenuSelector ].
	self on: aModel
]

{ #category : #'model protocol' }
RubPluggableTextMorph >> on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel setSelection: setSelectionSel [
	self
		on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: getSelectionSel
		menu: getMenuSel.
	setSelectionSelector := setSelectionSel.
	self borderWidth: 1.
	self setText: self getTextFromModel.
	self setSelection: self getSelectionFromModel
]

{ #category : #accessing }
RubPluggableTextMorph >> promptForCancel [
	"Ask for the appropriate action to take when unaccepted contents
	would be overwritten."

	| choice |
	choice := UIManager default
		confirm: 'Contents has been modified.\What do you want to do?' translated withCRs
		trueChoice: 'Accept' translated
		falseChoice: 'Discard' translated
		cancelChoice: 'Cancel' translated
		default: nil.
	choice
		ifNotNil: [ 
			choice
				ifTrue: [ 
					self acceptTextInModel.
					self basicHasUnacceptedEdits: false ]
				ifFalse: [ self model 
						ifNotNil: [ :m | m clearUserEditFlag ] 
						ifNil: [ self basicHasUnacceptedEdits: false ] ] ]
]

{ #category : #'accessing scrollbars' }
RubPluggableTextMorph >> registerScrollChanges: aSelector [

	self announcer 
		when: PaneScrolling 
		send: aSelector
		to: self model
]

{ #category : #'accessing scrollbars' }
RubPluggableTextMorph >> scrollerOffsetChanged [
	super scrollerOffsetChanged.
	self announcer
		announce:
			(PaneScrolling new
				step: self scrollPane hScrollbarValue @ self scrollPane vScrollbarValue;
				scrollPane: self scrollPane;
				yourself)
]

{ #category : #scrolling }
RubPluggableTextMorph >> styled: aBoolean [
	aBoolean ifTrue: [ ^ self beForSmalltalkCode  ].
	self beForPlainText
]

{ #category : #updating }
RubPluggableTextMorph >> update: aSymbol [
	aSymbol ifNil: [ ^ self ].
	aSymbol == #flash
		ifTrue: [ ^ self flash ].
	(aSymbol == self getColorSelector)
		ifTrue: [^ self backgroundColor: self getBackgroundColorFromModel ].
	aSymbol == self getTextSelector
		ifTrue: [ self updateTextWith: self getTextFromModel.
			^ self setSelection: self getSelectionFromModel ].
	aSymbol == self getSelectionSelector
		ifTrue: [ ^ self setSelection: self getSelectionFromModel ].
	aSymbol == #clearUserEdits
		ifTrue: [ ^ self hasUnacceptedEdits: false ].
	aSymbol == #wantToChange
		ifTrue: [ self canDiscardEdits
				ifFalse: [ ^ self promptForCancel ].
			^ self ].
	aSymbol == #appendEntry
		ifTrue:
			[ "Hack to manage the Transcript (Gloups) "
			self textArea
				handleEdit:
					[ "Knock off first half of text"
					self text size > self model characterLimit
						ifTrue: [ self textArea selectInvisiblyFrom: 1 to: self text size // 2.
							self textArea replaceSelectionWith: Text new ].	
					self appendText: self model contents asText ].
			^ self ].
	aSymbol == #clearText
		ifTrue: [ self textArea handleEdit: [ self updateTextWith: Text new ].
			^ self ].
	aSymbol == #bs
		ifTrue: [ self handleEdit: [ self updateTextWith: (self text copyFrom: 1 to: (self text size - 1 max: 0)) ].
			^ self ].
	aSymbol == #codeChangedElsewhere
		ifTrue: [ self hasEditingConflicts: true.
			^ self ].
	aSymbol == self getEnabledSelector
		ifTrue: [ self updateEnabled ].
	^ self
]

{ #category : #updating }
RubPluggableTextMorph >> updateEnabled [
	"Update the enablement state."

	self model ifNotNil: [
		self getEnabledSelector ifNotNil: [
			self enabled: (self model perform: self getEnabledSelector)]]
]
