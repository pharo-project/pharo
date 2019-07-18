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
		'default',
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

{ #category : #'model protocol' }
RubPluggableTextFieldMorph >> convertTo: aClass [

	"Set the converter object class."

	self converter
		ifNil: [ self converter: ( ObjectStringConverter forClass: aClass ) ]
		ifNotNil: [ self converter objectClass: aClass ]
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
	( newObj isNil and: [ self converter isNil ] )
		ifTrue: [ ^ Text new ].
	self converter ifNil: [ self convertTo: String ].
	^ ( self converter objectAsString: newObj ) ifNil: [ '' ] ifNotNil: [ :s | s shallowCopy ]
]

{ #category : #'model protocol' }
RubPluggableTextFieldMorph >> hasValidText [
	"Return true if the text is valid with respect to the converter."

	self converter ifNil: [^ super hasValidText].
	self checkTextWithConverter.
	^ super hasValidText
]

{ #category : #'model protocol' }
RubPluggableTextFieldMorph >> model: anObject [
	super model: anObject.
	anObject addDependent: self.
	anObject addDependent: self textArea
]

{ #category : #'set and get' }
RubPluggableTextFieldMorph >> setTextAndAccept: aText [ 
	
	self 
		setText: aText;
		hasUnacceptedEdits: true;
		accept
]

{ #category : #'event handling' }
RubPluggableTextFieldMorph >> textChanged [
	super textChanged.
	self checkTextWithConverter.
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

{ #category : #updating }
RubPluggableTextFieldMorph >> updateEnabled [
	"Update the enablement state."

	self model ifNotNil: [
		self getEnabledSelector ifNotNil: [
			self enabled: (self model perform: self getEnabledSelector)]]
]

{ #category : #'event handling' }
RubPluggableTextFieldMorph >> whenReturnEntered: anAnnouncement [
	super whenReturnEntered: anAnnouncement.
	crAction ifNotNil: [ crAction cull: nil ]
]
