"
Dialog style window with no window controls (expand, collapse etc). Usually opened modally (the morph that is used to modally open determines the modal scope, use of World implies ""system modal"").
Designed to be subclassed with content.
Supports Escape key for cancel and Enter key for default button.
"
Class {
	#name : #DialogWindow,
	#superclass : #StandardWindow,
	#instVars : [
		'cancelled'
	],
	#category : 'Morphic-Widgets-Windows-Widgets'
}

{ #category : #actions }
DialogWindow >> acceptTextMorphs [
	"Accept any text morphs except for those that have no edits."

	self allMorphs do: [:p |
		((p respondsTo: #accept) and: [
			(p respondsTo: #hasUnacceptedEdits) and: [
				p hasUnacceptedEdits]]) ifTrue: [p accept]]
]

{ #category : #'top window' }
DialogWindow >> activate [
	"Set the default focus for now, will want to
	remember it at some point."
	
	super activate.
	self world ifNil: [^self].
	self rememberedKeyboardFocus
		ifNil: [self defaultFocusMorph ifNotNil: [:m |
				m takeKeyboardFocus]]
]

{ #category : #theme }
DialogWindow >> activeFillStyle [
	"Return the active fillStyle for the receiver."
	
	^self theme dialogWindowActiveFillStyleFor: self
]

{ #category : #actions }
DialogWindow >> addInitialPanel [
	"Add the panel."

	self addMainPanel
]

{ #category : #actions }
DialogWindow >> addMainPanel [
	"Add the main panel."

	self addMorph: self newMainPanel frame: (0@0 corner: 1@1)
]

{ #category : #theme }
DialogWindow >> animateClose [
	"Animate closing."
]

{ #category : #actions }
DialogWindow >> applyChanges [
	"Apply the changes."

	self acceptTextMorphs
]

{ #category : #actions }
DialogWindow >> buttons [
	"Answer the buttons in the button row"

	^self paneMorphs last lastSubmorph submorphs
]

{ #category : #testing }
DialogWindow >> canBeMaximized [
	"Answer whether we are not we can be maximised."

	^self isResizeable
		ifTrue: [super canBeMaximized]
		ifFalse: [false]
]

{ #category : #actions }
DialogWindow >> cancel [
	"Cancel and close."

	self close
]

{ #category : #accessing }
DialogWindow >> cancelled [
	"Answer the value of cancelled"

	^ cancelled
]

{ #category : #accessing }
DialogWindow >> cancelled: anObject [
	"Set the value of cancelled"

	cancelled := anObject
]

{ #category : #actions }
DialogWindow >> close [
	"Close the window."

	self delete
]

{ #category : #keymapping }
DialogWindow >> closeWindowAction [
"closed by keyboard shortcut - act like 'cancel' "
	self cancel
]

{ #category : #actions }
DialogWindow >> defaultButton [
	"Answer the default button."

	^self
		findDeepSubmorphThat: [:m |
			(m isKindOf: PluggableButtonMorph) and: [m isDefault]]
		ifAbsent: [] 
]

{ #category : #'focus handling' }
DialogWindow >> defaultFocusMorph [
	"Answer the morph that should have the keyboard
	focus by default when the dialog is opened."
	
	^self defaultButton
		ifNil: [(self respondsTo: #nextMorphWantingFocus)
					ifTrue: [	self nextMorphWantingFocus]]
		ifNotNil: [:b | b enabled ifTrue: [b]]
]

{ #category : #testing }
DialogWindow >> defaultIsResizeable [

	^ false
]

{ #category : #actions }
DialogWindow >> defaultLabel [
	"Answer the default label for the receiver."

	^'Dialog' translated
]

{ #category : #'event handling' }
DialogWindow >> doubleClick: event [
	"Handle a double click. Maximize/restore the window.
	Not for dialogs if not resizeable..."
	
	self isResizeable ifTrue: [super doubleClick: event]
]

{ #category : #actions }
DialogWindow >> escapePressed [
	"Default is to cancel."

	self cancel
]

{ #category : #'event handling' }
DialogWindow >> handlesKeyboard: evt [
	"Return true if the receiver wishes to handle the given keyboard event"
	
	(super handlesKeyboard: evt) ifTrue: [^true].
	^evt keyCharacter = Character escape or: [
		(self defaultButton notNil and: [
			evt keyCharacter = Character cr])]
	
]

{ #category : #theme }
DialogWindow >> inactiveFillStyle [
	"Return the active fillStyle for the receiver."
	
	^self theme dialogWindowInactiveFillStyleFor: self
]

{ #category : #open/close }
DialogWindow >> initialExtent [
	"Answer the default extent for the receiver."

	|rl paneExt ext|
	rl := self getRawLabel.
	paneExt := self mainPanel
		ifNil: [0@0]
		ifNotNil: [:pane | pane minExtent].
	ext := paneExt + (2@ self labelHeight) + (2 * self class borderWidth)
		max: rl extent + 20.
	self isResizeable ifTrue: [
		self title: self title "adjust minimumExtent".
		self minimumExtent: (ext x max: self minimumExtent x)@(ext y max: self minimumExtent y)].
	^ext
]

{ #category : #initialization }
DialogWindow >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		cancelled: true;
		addInitialPanel
]

{ #category : #initialization }
DialogWindow >> initializeLabelArea [
	"Initialize the label area (titlebar) for the window."
	
	super initializeLabelArea.
	self removeBoxes.
	self replaceBoxes
]

{ #category : #'event handling' }
DialogWindow >> keyStroke: evt [
	"Check for return and escape keys."

	super keyStroke: evt.
	(self defaultButton notNil and: [evt keyCharacter = Character cr]) ifTrue: [self returnPressed. ^true].
	evt keyCharacter = Character escape ifTrue: [self escapePressed. ^true].
	^false
]

{ #category : #'event handling' }
DialogWindow >> keyboardFocusChange: aBoolean [
	"Set the focus to the default button."
	super keyboardFocusChange: aBoolean.
	aBoolean ifTrue: [
		self defaultFocusMorph ifNotNil: [:b |
			b takeKeyboardFocus]]
]

{ #category : #actions }
DialogWindow >> mainPanel [
	"Anwer the main panel morph or nil if not yet present."

	^self paneMorphs isEmpty
		ifFalse: [self paneMorphs first]
]

{ #category : #actions }
DialogWindow >> newButtonRow [
	"Answer a new ok/cancel button row."

	|answer buttons e|
	buttons := self newButtons.
	e := 0@0.
	buttons do: [:b | e := e max: b minExtent].
	buttons do: [:b | b extent: e].
	answer := Morph new
		color: Color transparent;
		changeTableLayout;
		cellInset: 8;
		listDirection: #leftToRight;
		listCentering: #bottomRight;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.
	buttons do: [:b | answer addMorphBack: b].
	^answer
]

{ #category : #actions }
DialogWindow >> newButtons [
	"Answer new buttons as appropriate."

	^{self newOKButton isDefault: true. self newCancelButton}
]

{ #category : #actions }
DialogWindow >> newContentMorph [
	"Answer a new content morph."

	^Morph new
		color: Color transparent;
		hResizing: #spaceFill;
		vResizing: #spaceFill
]

{ #category : #actions }
DialogWindow >> newMainPanel [
	"Answer a new main panel."

	^self newDialogPanel
		addMorphBack: self newContentMorph;
		addMorphBack: self newButtonRow;
		yourself
]

{ #category : #actions }
DialogWindow >> ok [
	"Apply the changes and close."

	self
		cancelled: false;
		applyChanges;
		delete
]

{ #category : #theme }
DialogWindow >> preferredCornerStyle [
	"Answer the preferred corner style."

	^self theme dialogWindowPreferredCornerStyleFor: self
]

{ #category : #actions }
DialogWindow >> returnPressed [
	"Default is to do the default button."

	(self defaultButton ifNil: [^self]) performAction
]

{ #category : #actions }
DialogWindow >> setButtonFont: aFont [
	"Set the font for the buttons."

	| buttons e|
	buttons := self buttons.
	e := 0@0.
	buttons do: [:b | | hRes vRes |
		hRes := b hResizing.
		vRes := b vResizing.
		b
			hResizing: #shrinkWrap;
			vResizing: #shrinkWrap.
		b label: b label font: aFont.
		e := e max: b minExtent.
		b
			hResizing: hRes;
			vResizing: vRes].
	buttons do: [:b | b extent: e]
]

{ #category : #initialization }
DialogWindow >> setFramesForLabelArea [
	"Delegate to theme."
	
	self theme configureDialogWindowLabelAreaFrameFor: self
]

{ #category : #actions }
DialogWindow >> setLabelWidgetAllowance [
	"Set the extra space required, in general, apart from the label.
	No extra needed for dialogs."
	
	^labelWidgetAllowance :=  0
]

{ #category : #controls }
DialogWindow >> title: aString [
	"Set the window title."
	
	super title: aString.
	label fitContents.
	self minimumExtent: (((label width + 20 min: (Display width // 2)) max: self minimumExtent x)@ self minimumExtent y)
]

{ #category : #theme }
DialogWindow >> wantsRoundedCorners [
	"Answer whether rounded corners are wanted."
	
	^(self theme dialogWindowPreferredCornerStyleFor: self) == #rounded
]
