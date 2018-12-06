Extension { #name : #PluggableTextMorph }

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> adoptPaneColor: paneColor [
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self borderStyle baseColor: (self enabled ifTrue: [paneColor twiceDarker] ifFalse: [paneColor darker]).
	self fillStyle: self fillStyleToUse.
	self borderWidth > 0 
		ifTrue: [self borderStyle: self borderStyleToUse]
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> appendText: aTextOrString [
	"Append the given text to the receiver."

	self handleEdit: [
		self 
			selectInvisiblyFrom: textMorph asText size + 1 to: textMorph asText size;
			replaceSelectionWith: aTextOrString;
			selectFrom: textMorph asText size + 1 to: textMorph asText size;
			hasUnacceptedEdits: false;
			scrollSelectionIntoView;
			changed]
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> focusBounds [
	"Answer the bounds for drawing the focus indication
	(when externalFocusForPluggableText is enabled)."

	^self theme textFocusBoundsFor: self
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> hasKeyboardFocus [
	"Answer whether the receiver has keyboard focus."

	^super hasKeyboardFocus or: [(self textMorph ifNil: [^false]) hasKeyboardFocus]
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> hasValidText [
	"Return true if the text is valid for acceptance."

	^true
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> keyboardFocusChange: aBoolean [
	"Pass on to text morph."
	super keyboardFocusChange: aBoolean.
	aBoolean ifTrue: [self textMorph takeKeyboardFocus]
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> layoutBounds: aRectangle [
	"Set the bounds for laying out children of the receiver.
	Note: written so that #layoutBounds can be changed without touching this method"
	
	super layoutBounds: aRectangle.
	textMorph ifNotNil:
		[textMorph extent: (self innerExtent x)@self height].
	self setScrollDeltas

]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> minHeight [
	"Implemented here since extent: overriden."

	^super minHeight max: 16
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> minWidth [
	"Implemented here since extent: overriden."

	^super minWidth max: 36
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> navigationKey: event [
	"Check for tab key activity and change focus as appropriate.
	Must override here rather than in #tabKey: otherwise
	the tab will get passed to the window and change the focus."

	(event keyCharacter = Character tab and: [
		(event anyModifierKeyPressed or: [event shiftPressed]) not]) ifTrue: [^false].
	^super navigationKey: event
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^true
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> themeChanged [
	self color: self defaultColor.
	super themeChanged
]

{ #category : #'*Polymorph-Widgets-Core' }
PluggableTextMorph >> wantsKeyboardFocus [
	"Answer whether the receiver would like keyboard focus
	in the general case (mouse action normally). Even if disabled
	we allow for text morphs since can potentially copy text."

	^self takesKeyboardFocus and: [
		self visible and: [
			self enabled or: [self valueOfProperty: #wantsKeyboardFocusWhenDisabled ifAbsent: [true]]]]

]
