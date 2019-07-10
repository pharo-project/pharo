"
I am a morph that captures keymappings, so they can be used for configuration/customization, or debugging.
"
Class {
	#name : #KMCatcherMorph,
	#superclass : #BorderedMorph,
	#instVars : [
		'focused',
		'keystrokes',
		'keymapSetting',
		'labelMorph',
		'initialShortcut',
		'edited'
	],
	#category : #'Keymapping-Settings'
}

{ #category : #'instance creation' }
KMCatcherMorph class >> for: aModel [
	^ (self new)
		keymapSetting: aModel;
		yourself
]

{ #category : #api }
KMCatcherMorph >> accept [
	| shortcut |
	shortcut := self shortcut.
	keymapSetting accept: shortcut.
	self initialShortcut: shortcut.
	edited := false.
]

{ #category : #'event handling' }
KMCatcherMorph >> allowsKeymapping [
	^ false
]

{ #category : #accessing }
KMCatcherMorph >> balloonText [
	^ 'Click to edit shortcut.  Right click to open context menu.'
]

{ #category : #api }
KMCatcherMorph >> cancel [
	edited
		ifFalse: [ ^ self ].
	self keystrokes: OrderedCollection new.
	self showInitialShortcut.
	self initializeKeystrokes
]

{ #category : #'event handling' }
KMCatcherMorph >> clear [
	self keystrokes removeAll.
	self showKeystrokes.
]

{ #category : #drawing }
KMCatcherMorph >> drawOn: aCanvas [
	"Indicate unaccepted edits, conflicts etc."

	super drawOn: aCanvas. 
	focused ifTrue: [
		self theme drawTextAdornmentFor: self color: Color orange on: aCanvas]
]

{ #category : #'event handling' }
KMCatcherMorph >> handlesKeyboard: event [
	^ focused
]

{ #category : #'event handling' }
KMCatcherMorph >> handlesMouseDown: event [
	^ true
]

{ #category : #initialization }
KMCatcherMorph >> initialShortcut: aShortcut [
	initialShortcut := aShortcut.
	self showInitialShortcut.
	self initializeKeystrokes.
]

{ #category : #initialization }
KMCatcherMorph >> initialize [
	super initialize.
	self layoutPolicy: TableLayout new.
	self listCentering: #center.
	self width: 300.
	self height: 25.
	self color: (self theme textEditorDisabledFillStyleFor: self).
	self borderStyle: (self theme textEditorDisabledBorderStyleFor: self).
	labelMorph := StringMorph contents: ''.
	self addMorph: labelMorph.
	edited := false.
	focused := false.
	self initializeKeystrokes.
	
]

{ #category : #initialization }
KMCatcherMorph >> initializeKeystrokes [
	keystrokes := OrderedCollection new.
]

{ #category : #'event handling' }
KMCatcherMorph >> keyStroke: aKeyStroke [ 
	edited := true.
	self keystrokes add: aKeyStroke.
	self showKeystrokes.
]

{ #category : #'event handling' }
KMCatcherMorph >> keyboardFocusChange: aBoolean [
	super keyboardFocusChange: aBoolean.
	focused := aBoolean.
	focused
		ifTrue: [ self color: (self theme textEditorNormalFillStyleFor: self) ]
		ifFalse: [ self color: (self theme textEditorDisabledFillStyleFor: self) ].
	^ true
]

{ #category : #'event handling' }
KMCatcherMorph >> keymapSetting: aSetting [
	keymapSetting := aSetting.
	self initialShortcut: keymapSetting shortcut.
]

{ #category : #api }
KMCatcherMorph >> keystrokes [
	^ keystrokes
]

{ #category : #api }
KMCatcherMorph >> keystrokes: someKeystrokes [
	keystrokes := someKeystrokes.
	self showKeystrokes.
]

{ #category : #'event handling' }
KMCatcherMorph >> mouseDown: event [ 
	super mouseDown: event.
	event yellowButtonPressed 
		ifTrue: [ self showContextMenu ]
		ifFalse: [ self currentWorld activeHand newKeyboardFocus: self ]
]

{ #category : #accessing }
KMCatcherMorph >> setMessage: aMessage [ 
	labelMorph contents: aMessage
]

{ #category : #api }
KMCatcherMorph >> shortcut [
	| theKeystrokes |
	theKeystrokes := self keystrokes.
	theKeystrokes 
		ifEmpty: [ ^ KMNoShortcut new ].
		
	^ theKeystrokes allButFirst 
		inject: theKeystrokes first asKeyCombination 
		into: [ :acum :each | 
			acum , each asKeyCombination ]
]

{ #category : #api }
KMCatcherMorph >> shortcutString [
	| shortcut |
	self keystrokes 
		ifEmpty: [ ^ '' ].
	shortcut := self shortcut.
	^ shortcut asString
]

{ #category : #'event handling' }
KMCatcherMorph >> showContextMenu [
	| menu |
	menu := MenuMorph new defaultTarget: self.
	
	edited 
		ifTrue: [
			menu add: 'accept' selector: #accept.
			menu add: 'clear' selector: #clear.
			menu add: 'cancel' selector: #cancel.
			].
		
	menu popUpInWorld
]

{ #category : #initialization }
KMCatcherMorph >> showInitialShortcut [
	self setMessage: initialShortcut asString.
]

{ #category : #private }
KMCatcherMorph >> showKeystrokes [
	self setMessage: self shortcutString.
]

{ #category : #'halos and balloon help' }
KMCatcherMorph >> wantsBalloon [
	^ true
]
