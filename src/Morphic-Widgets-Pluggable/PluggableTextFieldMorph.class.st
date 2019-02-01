"
Single-line text field editor with DialogWindow key integration (return for default, escape for cancel) and keyboard focus navigation (tab/shift-tab). Additionally supports pluggable converters to translate between an object and its string form and vice-versa.
"
Class {
	#name : #PluggableTextFieldMorph,
	#superclass : #PluggableTextMorph,
	#instVars : [
		'converter',
		'ghostText',
		'default',
		'entryCompletion',
		'endRow',
		'textMorphClass',
		'textFont'
	],
	#category : #'Morphic-Widgets-Pluggable-Text'
}

{ #category : #'model access' }
PluggableTextFieldMorph >> acceptTextInModel [
	"Inform the model that the receiver's textMorph's
	text should be accepted.
	Answer true if the model accepted ok, false
	otherwise"
	| objectToAccept text |
	self hasValidText ifFalse: [^false].
	text := self text.
	objectToAccept := self converter
				ifNil: [text]
				ifNotNil: [self default
						ifNil: [self converter stringAsObject: text asString]
						ifNotNil: [text
								ifEmpty: [self default]
								ifNotEmpty: [self converter stringAsObject: text asString]]].
	^ setTextSelector isNil
		or: [setTextSelector numArgs = 2
				ifTrue: [model
						perform: setTextSelector
						with: objectToAccept
						with: self]
				ifFalse: [model perform: setTextSelector with: objectToAccept]]
]

{ #category : #encryption }
PluggableTextFieldMorph >> beDecrypted [

	self textMorph font: TextStyle defaultFont.
]

{ #category : #encryption }
PluggableTextFieldMorph >> beEncrypted [

	self textMorph font: (StrikeFont passwordFontSize: self theme textFont pointSize).
]

{ #category : #accessing }
PluggableTextFieldMorph >> borderStyleToUse [
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme textFieldNormalBorderStyleFor: self]
		ifFalse: [self theme textFieldDisabledBorderStyleFor: self]
]

{ #category : #testing }
PluggableTextFieldMorph >> chooserHasFocus [
	^ entryCompletion notNil and: [entryCompletion chooser notNil and: [entryCompletion chooser hasKeyboardFocus]]
]

{ #category : #accessing }
PluggableTextFieldMorph >> closeChooser [	
	entryCompletion
		ifNotNil: [entryCompletion closeChooser]
]

{ #category : #'model access' }
PluggableTextFieldMorph >> convertTo: aClass [
	"Set the converter object class."

	self converter isNil
		ifTrue: [self converter: (ObjectStringConverter forClass: aClass)]
		ifFalse: [self converter objectClass: aClass]
]

{ #category : #accessing }
PluggableTextFieldMorph >> converter [
	"Answer the value of converter"

	^ converter
]

{ #category : #accessing }
PluggableTextFieldMorph >> converter: anObject [
	"Set the value of converter"

	converter := anObject
]

{ #category : #accessing }
PluggableTextFieldMorph >> default [
	^ default
]

{ #category : #accessing }
PluggableTextFieldMorph >> default: anObject [
	default := anObject
]

{ #category : #drawing }
PluggableTextFieldMorph >> drawOn: aCanvas [ 
	"Eventually draw the helpText"
	super drawOn: aCanvas.
	self ghostText
		ifNotEmpty: [
			self text isEmpty
				ifTrue: [| ghostBounds |
					ghostBounds := ((self scroller submorphBounds topLeft corner: self innerBounds bottomRight)
						translateBy: 0 @ (self scroller height - self theme textFont height / 2) rounded)
						insetBy: self textMarginsWidth @ 0.
					aCanvas 
						drawString: self ghostText
						in: ghostBounds
						font: self theme textFont
						color: self theme ghostTextColor]].
	entryCompletion 
		ifNotNil: [entryCompletion closeIfNotNeeded: self].
]

{ #category : #encryption }
PluggableTextFieldMorph >> encrypted: aBoolean [

	aBoolean
		ifTrue: [ self beEncrypted ]
		ifFalse: [ self beDecrypted ]
]

{ #category : #accessing }
PluggableTextFieldMorph >> entryCompletion: anEntryCompletion [
	entryCompletion := anEntryCompletion.
	entryCompletion 
		ifNotNil: [ entryCompletion chooseBlock isNil
			ifTrue: [entryCompletion chooseBlock: [:v | self setText: v. self acceptTextInModel ]]
			ifFalse: [ | blk | 
				blk := entryCompletion chooseBlock.
				entryCompletion chooseBlock: [:v | self setText: v. self acceptTextInModel. blk value: v]]].

]

{ #category : #accessing }
PluggableTextFieldMorph >> fillStyleToUse [
	"Answer the fillStyle that should be used for the receiver."
	self color ifNotNil: [ ^ self color ].
	^self enabled
		ifTrue: [self theme textFieldNormalFillStyleFor: self]
		ifFalse: [self theme textFieldDisabledFillStyleFor: self]
]

{ #category : #focus }
PluggableTextFieldMorph >> focusChanged [
	(self hasFocus or: [self chooserHasFocus])
		ifFalse: [self closeChooser].
	super focusChanged

]

{ #category : #'model access' }
PluggableTextFieldMorph >> getSelection [
	"Answer the model's selection interval.
	If not available keep the current selection."

	getSelectionSelector isNil ifFalse: [^super getSelection].
	^selectionInterval ifNil: [super getSelection]
]

{ #category : #'model access' }
PluggableTextFieldMorph >> getText [
	"Retrieve the current model text. Set the converter to
	convert between the class of the returned object and string form."

	| newObj |
	getTextSelector isNil ifTrue: [^super getText].
	newObj := model perform: getTextSelector.
	(newObj isNil and: [self converter isNil]) ifTrue: [^Text new].
	self converter isNil
		ifTrue: [self convertTo: newObj class].
	^(self converter objectAsString: newObj)
		ifNil: ['']
		ifNotNil: [:s | s shallowCopy]
]

{ #category : #accessing }
PluggableTextFieldMorph >> ghostText [
	"Answer the value of helpText"

	^ ghostText ifNil: [ghostText := '']
]

{ #category : #accessing }
PluggableTextFieldMorph >> ghostText: aText [
	"Set the value of helpText"
	ghostText := aText ifNotNil: [aText asString]

]

{ #category : #'event handling' }
PluggableTextFieldMorph >> handlesMouseWheel: evt [
	"Do I want to receive mouseWheel events?." 

	^false
]

{ #category : #accessing }
PluggableTextFieldMorph >> hasUnacceptedEdits: aBoolean [
	"Set the hasUnacceptedEdits flag to the given value. "
	
	aBoolean == hasUnacceptedEdits 
		ifFalse: [hasUnacceptedEdits := aBoolean. 
			self changed].
	aBoolean 
		ifFalse: [hasEditingConflicts := false].
	self okToStyle 
		ifTrue: [ self styler styleInBackgroundProcess: textMorph contents]
]

{ #category : #testing }
PluggableTextFieldMorph >> hasValidText [
	"Return true if the text is valid with respect to the converter."

	|string|
	self converter ifNil: [^super hasValidText].
	string := self text asString.
	^(string isEmpty and: [self default notNil]) or: [
		self converter isStringValid: string]
]

{ #category : #initialization }
PluggableTextFieldMorph >> initialize [

	textMorphClass := TextMorphForFieldView.
	super initialize.
	self beDecrypted
]

{ #category : #geometry }
PluggableTextFieldMorph >> innerBounds [
	| inb |
	inb := super innerBounds.
	endRow 
		ifNotNil: [inb := inb withRight: (inb right - endRow fullBounds width)].
	^ inb
]

{ #category : #focus }
PluggableTextFieldMorph >> keyboardFocusChange: aBoolean [
	self closeChooser.
	super keyboardFocusChange: aBoolean.

]

{ #category : #'event handling' }
PluggableTextFieldMorph >> keystrokeFromTextMorph: anEvent [
	anEvent keyCharacter = Character cr
		ifTrue: [self closeChooser].
	^ (super keystrokeFromTextMorph: anEvent)
		or: [entryCompletion notNil and: [entryCompletion keystroke: anEvent from: self]]
			
	
]

{ #category : #initialization }
PluggableTextFieldMorph >> layoutChanged [
	super layoutChanged.
	endRow 
		ifNotNil: [ | deltaY |
			deltaY := (self height - endRow fullBounds height) // 2.
			endRow position: self boundsInWorld topRight - (endRow fullBounds width @ deltaY negated)].

]

{ #category : #accessing }
PluggableTextFieldMorph >> maxLength [
	"Answer the maximum number of characters that may be typed."

	^self textMorph maxLength
]

{ #category : #accessing }
PluggableTextFieldMorph >> maxLength: anInteger [
	"Set the maximum number of characters that may be typed."

	self textMorph maxLength: anInteger
]

{ #category : #'event handling' }
PluggableTextFieldMorph >> mouseDownFromTextMorph: anEvent [
	super mouseDownFromTextMorph: anEvent.
	entryCompletion ifNotNil: [entryCompletion mouseDownFromTextMorph: anEvent]
]

{ #category : #styling }
PluggableTextFieldMorph >> okToStyle [
	^ false
]

{ #category : #accessing }
PluggableTextFieldMorph >> openChooser [	
	entryCompletion
		ifNotNil: [entryCompletion openChooserWith: textMorph text string from: self]
]

{ #category : #initialization }
PluggableTextFieldMorph >> outOfWorld: aWorld [
	self closeChooser.
	super outOfWorld: aWorld
]

{ #category : #'event handling' }
PluggableTextFieldMorph >> scrollByKeyboard: event [ 
	"If event is ctrl+up/down then scroll and answer true.
	Just don't, really!"
	
	^false
]

{ #category : #'editor access' }
PluggableTextFieldMorph >> scrollSelectionIntoView: event [ 
	"Scroll my text into view if necessary and return true, else return false.
	Redone here to deal with horizontal scrolling!"
	
	| selRects delta selRect rectToTest transform cpHere |
	self selectionInterval: textMorph editor selectionInterval.
	selRects := textMorph paragraph selectionRects.
	selRects isEmpty ifTrue: [^ false].
	rectToTest := selRects first merge: selRects last.
	transform := scroller transformFrom: self.
	(event notNil and: [event anyButtonPressed]) ifTrue:  "Check for autoscroll"
		[cpHere := transform localPointToGlobal: event cursorPoint.
		cpHere x <= self left
			ifTrue: [rectToTest := selRects first topLeft extent: 2@2]
			ifFalse: [cpHere x >= self right
					ifTrue: [rectToTest := selRects last bottomRight extent: 2@2]
					ifFalse: [^ false]]].
	selRect := transform localBoundsToGlobal: rectToTest.
	selRect width > bounds width
		ifTrue: [^ false].  "Would not fit, even if we tried to scroll"
	(delta := selRect amountToTranslateWithin: self innerBounds) ~= (0@0) ifTrue:
		["Scroll end of selection into view if necessary"
		self scrollBy: delta truncated.
		^ true].
	^ false
]

{ #category : #'event handling' }
PluggableTextFieldMorph >> textChanged [
	super textChanged.
	self openChooser
]

{ #category : #accessing }
PluggableTextFieldMorph >> textFont [
	"We should clean"
	
	^ textFont ifNil: [ StandardFonts defaultFont  ]
]

{ #category : #accessing }
PluggableTextFieldMorph >> textFont: aFont [
	"We should clean"
	
	textFont := aFont
]

{ #category : #accessing }
PluggableTextFieldMorph >> textMarginsWidth [
	
	^ self textMorph margins asMargin left
	
]

{ #category : #private }
PluggableTextFieldMorph >> textMorphClass [
	"Answer the class used to create the receiver's textMorph"
	
	^ textMorphClass
]

{ #category : #updating }
PluggableTextFieldMorph >> update: aSymbol [
	"Update the receiver based on the given aspect.
	Override to not accept an #appendText for a text field
	since if broadcast by a model it will append to ALL
	text fields/editors."
	
	aSymbol == #appendEntry
		ifTrue: [^self].
	^super update: aSymbol
]

{ #category : #initialization }
PluggableTextFieldMorph >> withDropListButton [
	| downArrow downArrowBtn |
	endRow := AlignmentMorph newRow.
	downArrow := (self theme 
		basicCreateArrowOfDirection: #bottom 
		size: (textMorph textStyle defaultFont height)
		color: Color veryLightGray).
	downArrowBtn := IconicButton new labelGraphic: downArrow.
	downArrowBtn height: self innerBounds height.
	downArrowBtn borderWidth: 0.
	downArrowBtn color: Color transparent.
	downArrowBtn actionSelector:  #openChooserWithAllOrCloseFrom:.
	downArrowBtn target: entryCompletion.
	downArrowBtn arguments: (Array with: self).
	endRow height: downArrowBtn height.
	endRow width: downArrowBtn width.
	endRow color: Color transparent.
	endRow addMorph: downArrowBtn.
	self addMorph: endRow.
]
