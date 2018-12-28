"
This class was introduced to ease the migration to Rubric.
Thus, I''am a replacement for compatibility with old PluggableTextFieldMorph.
It is still partly based on change/update.
Do not use this class for new developments since it will be deprecated asap.
Use RubTextFieldMorph instead

"
Class {
	#name : #RubPluggableTextFieldMorph,
	#superclass : #RubTextFieldMorph,
	#instVars : [
		'askBeforeDiscardingEdits',
		'default',
		'entryCompletion',
		'endRow',
		'getEnabledSelector',
		'converter',
		'crAction',
		'changedAction'
	],
	#category : #'Rubric-Compatibility-toBeDeprecated'
}

{ #category : #'model protocol' }
RubPluggableTextFieldMorph >> acceptTextInModel [
	"Inform the model that the receiver's textArea text should be accepted.
	Answer true if the model accepted ok, false otherwise"
	| objectToAccept text result |
	self hasValidText ifFalse: [ ^ false].
	text := self text.
	objectToAccept := self converter
				ifNil: [text]
				ifNotNil: [self default
						ifNil: [self converter stringAsObject: text asString]
						ifNotNil: [text
								ifEmpty: [self default]
								ifNotEmpty: [self converter stringAsObject: text asString]]].
	result := setTextSelector isNil
		or: [setTextSelector numArgs = 2
				ifTrue: [self model
						perform: setTextSelector
						with: objectToAccept
						with: self]
				ifFalse: [self model perform: setTextSelector with: objectToAccept]].
	result == true ifTrue: [ self closeChooser ].
	^ result

]

{ #category : #accessing }
RubPluggableTextFieldMorph >> askBeforeDiscardingEdits: aBoolean [
	"Set the flag that determines whether the user should be asked before discarding unaccepted edits."

	askBeforeDiscardingEdits := aBoolean
]

{ #category : #encryption }
RubPluggableTextFieldMorph >> beDecrypted [

	self textMorph font: TextStyle defaultFont.
]

{ #category : #encryption }
RubPluggableTextFieldMorph >> beEncrypted [

	self textMorph font: (StrikeFont passwordFontSize: self theme textFont pointSize).
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> changedAction: aBlock [
	changedAction := aBlock
]

{ #category : #testing }
RubPluggableTextFieldMorph >> checkTextWithConverter [

	|string|
	self converter ifNil: [^ self].
	string := self text asString.
	(string isEmpty and: [self default notNil]) or: [
		hasValidText := self converter isStringValid: string]
]

{ #category : #testing }
RubPluggableTextFieldMorph >> chooserHasFocus [
	^ entryCompletion notNil and: [entryCompletion chooser notNil and: [entryCompletion chooser hasKeyboardFocus]]
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> closeChooser [	
	entryCompletion
		ifNotNil: [entryCompletion closeChooser]
]

{ #category : #'model protocol' }
RubPluggableTextFieldMorph >> convertTo: aClass [
	"Set the converter object class."

	self converter isNil
		ifTrue: [self converter: (ObjectStringConverter forClass: aClass)]
		ifFalse: [self converter objectClass: aClass]
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> converter [
	"Answer the value of converter"

	^ converter
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> converter: anObject [
	"Set the value of converter"

	converter := anObject
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> crAction: aMessageSend [
	crAction := aMessageSend
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> default [
	^ default
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> default: anObject [
	default := anObject
]

{ #category : #protocol }
RubPluggableTextFieldMorph >> disable [
	self beReadOnly 
]

{ #category : #drawing }
RubPluggableTextFieldMorph >> drawOn: aCanvas [ 
	"Eventually draw the helpText"
	super drawOn: aCanvas
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> encrypted: aBoolean [

	aBoolean
		ifTrue: [ self beEncrypted ]
		ifFalse: [ self beDecrypted ]
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> entryCompletion [ 
	^ entryCompletion 
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> entryCompletion: anEntryCompletion [
	entryCompletion := anEntryCompletion.
	entryCompletion 
		ifNotNil: [ entryCompletion chooseBlock isNil
			ifTrue: [entryCompletion chooseBlock: [:v | self setText: v. self acceptTextInModel ]]
			ifFalse: [ | blk | 
				blk := entryCompletion chooseBlock.
				entryCompletion chooseBlock: [:v | self setText: v. self acceptTextInModel. blk value: v]]].

]

{ #category : #focus }
RubPluggableTextFieldMorph >> focusChanged [
	(self hasFocus or: [self chooserHasFocus])
		ifFalse: [self closeChooser].
	super focusChanged

]

{ #category : #accessing }
RubPluggableTextFieldMorph >> getColorSelector [
	^ self getBackgroundColorSelector 
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> getColorSelector: aSymbol [
	self getBackgroundColorSelector: aSymbol.
	self update: self getColorSelector.
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> getEnabledSelector [
	"Answer the value of getEnabledSelector"

	^ getEnabledSelector
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> getEnabledSelector: aSymbol [
	"Answer the value of getEnabledSelector"

	getEnabledSelector := aSymbol
]

{ #category : #'acessing text' }
RubPluggableTextFieldMorph >> getTextFromModel [
	| newObj |
	newObj := self model perform: self getTextSelector withEnoughArguments: {self}.
	(newObj isNil and: [ self converter isNil ])
		ifTrue: [ ^ Text new ].
	self converter isNil
		ifTrue: [ self convertTo: String ].
	^ (self converter objectAsString: newObj) ifNil: [ '' ] ifNotNil: [ :s | s shallowCopy ]
]

{ #category : #'model protocol' }
RubPluggableTextFieldMorph >> hasValidText [
	"Return true if the text is valid with respect to the converter."

	self converter ifNil: [^ super hasValidText].
	self checkTextWithConverter.
	^ super hasValidText
]

{ #category : #initialization }
RubPluggableTextFieldMorph >> initialize [
	super initialize.
	self beDecrypted.
	askBeforeDiscardingEdits := true
]

{ #category : #focus }
RubPluggableTextFieldMorph >> keyboardFocusChange: aBoolean [
	self closeChooser.
	super keyboardFocusChange: aBoolean.

]

{ #category : #initialization }
RubPluggableTextFieldMorph >> layoutChanged [
	super layoutChanged.
	endRow 
		ifNotNil: [ | deltaY |
			deltaY := (self height - endRow fullBounds height) // 2.
			endRow position: self boundsInWorld topRight - (endRow fullBounds width @ deltaY negated)].

]

{ #category : #'model protocol' }
RubPluggableTextFieldMorph >> model: anObject [
	super model: anObject.
	anObject addDependent: self.
	anObject addDependent: self textArea
]

{ #category : #'model protocol' }
RubPluggableTextFieldMorph >> on: aModel text: aGetTextSelector accept: aSetTextSelector readSelection: aReadSelectionSelector menu: aGetMenuSelector [
	self getTextSelector: aGetTextSelector.
	self setTextSelector: aSetTextSelector.
	self getSelectionSelector: aReadSelectionSelector.
	aGetMenuSelector
		ifNil: [ self textArea forbidMenu ]
		ifNotNil: [ self menuProvider: aModel selector: aGetMenuSelector ].
	self on: aModel
]

{ #category : #'model protocol' }
RubPluggableTextFieldMorph >> on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel setSelection: setSelectionSel [
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
RubPluggableTextFieldMorph >> openChooser [	
	entryCompletion
		ifNotNil: [
			entryCompletion openChooserWith: self textArea text string from: self]
]

{ #category : #initialization }
RubPluggableTextFieldMorph >> outOfWorld: aWorld [
	self closeChooser.
	super outOfWorld: aWorld
]

{ #category : #'event handling' }
RubPluggableTextFieldMorph >> textChanged [
	super textChanged.
	self checkTextWithConverter.
	self updateChooser.
	changedAction ifNotNil: [changedAction value: self getText ].
]

{ #category : #updating }
RubPluggableTextFieldMorph >> update: aSymbol [
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

{ #category : #'event handling' }
RubPluggableTextFieldMorph >> updateChooser [
	entryCompletion
		ifNotNil: [  self openChooser ] 
]

{ #category : #updating }
RubPluggableTextFieldMorph >> updateEnabled [
	"Update the enablement state."

	self model ifNotNil: [
		self getEnabledSelector ifNotNil: [
			self enabled: (self model perform: self getEnabledSelector)]]
]

{ #category : #accessing }
RubPluggableTextFieldMorph >> wantsFrameAdornments: aBoolean [
	aBoolean ifTrue: [ self withAdornment  ] ifFalse: [ self withoutAdornment ]
]

{ #category : #'event handling' }
RubPluggableTextFieldMorph >> whenKeystrokeInTextArea: anAnnouncement [
	super whenKeystrokeInTextArea: anAnnouncement.
	entryCompletion ifNotNil: [entryCompletion keystroke: anAnnouncement event from: self]
]

{ #category : #'event handling' }
RubPluggableTextFieldMorph >> whenReturnEntered: anAnnouncement [
	super whenReturnEntered: anAnnouncement.
	crAction ifNotNil: [ crAction value ]
]

{ #category : #initialization }
RubPluggableTextFieldMorph >> withDropListButton [
	self withRulerNamed: #dropListButton
]

{ #category : #compat }
RubPluggableTextFieldMorph >> wrapFlag: aBoolean [
	self flag: #userShouldBeRevisited.
]
