"
I give a world its desktop behavior: the world menu, the menu of its windows and morphs, the selection rectangle when dragging on the background, and the quit dialog when its window is closed.

Add me to the world: World addRootEventHandler: DesktopRootEventHandler new.
"
Class {
	#name : 'DesktopRootEventHandler',
	#superclass : 'MorphRootEventHandler',
	#category : 'Morphic-Desktop',
	#package : 'Morphic-Desktop'
}

{ #category : 'clicking' }
DesktopRootEventHandler >> contentsMenuClick: evt [
	"Sent by the hand when the click on the background turned out to be a click"

	self openContentsMenu: evt in: evt hand world
]

{ #category : 'private' }
DesktopRootEventHandler >> dragThrough: evt in: aWorld [
	"Draw out a selection rectangle"
	| selection |
	selection := SelectionMorph newBounds: (evt cursorPoint extent: 8@8).
	aWorld addMorph: selection.
	selection extendByHand: evt hand.
	^ true
]

{ #category : 'event handling' }
DesktopRootEventHandler >> handleEvent: anEvent in: aWorld [
	"Take the clicks and drags on the background of aWorld, Escape when no morph has the keyboard focus, and the close of aWorld's window"

	anEvent isMouse ifTrue: [ ^ anEvent isMouseDown and: [ self takeMouseDown: anEvent in: aWorld ] ].
	anEvent isKeyboard ifTrue: [ ^ self takeKeystroke: anEvent in: aWorld ].
	anEvent isWindowEvent ifTrue: [ ^ self takeWindowEvent: anEvent in: aWorld ].
	^ false
]

{ #category : 'testing' }
DesktopRootEventHandler >> isBackground: aPoint of: aWorld [
	"Answer whether no morph of aWorld is at aPoint"

	^ aWorld submorphs noneSatisfy: [ :each | each fullContainsPoint: aPoint ]
]

{ #category : 'private' }
DesktopRootEventHandler >> openContentsMenu: evt in: aWorld [
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu |
	menu := aWorld morphicUIManager newMenuIn: aWorld for: aWorld.
	aWorld contentsMenu: menu.
	menu buildTitle: [ :titleMorph |
		titleMorph bigTitle: aWorld contentsMenuTitle
	];
	popUpEvent: evt in: (aWorld world ifNil: [aWorld]).
	^ true
]

{ #category : 'private' }
DesktopRootEventHandler >> openWorldMenu: evt in: aWorld [
	"Put up the world menu, triggered by the passed-in event."

	"If the user does not want a world menu, do not invoke it."
	aWorld class shouldShowWorldMenu ifFalse: [ ^ false ].

	aWorld bringTopmostsToFront.
	"put up screen menu"
	aWorld worldMenu popUpEvent: evt in: aWorld.
	^ true
]

{ #category : 'private' }
DesktopRootEventHandler >> quitOnWindowClose: anEvent in: aWorld [
	"The window close dialog is modal, since the windowClose event originates from the system window button, we need to protect against opening multiple, otherwise we end up with the world in a locked state when one of them is cancelled"

	(aWorld valueOfProperty: #canOpenCloseDialog ifAbsent: true) ifFalse: [ ^ true ].
	aWorld setProperty: #canOpenCloseDialog toValue: false.
	WorldState quitSession.
	aWorld removeProperty: #canOpenCloseDialog.
	^ true
]

{ #category : 'clicking' }
DesktopRootEventHandler >> selectionDrag: evt [
	"Sent by the hand when the click on the background turned out to be a drag"

	self dragThrough: evt in: evt hand world
]

{ #category : 'private' }
DesktopRootEventHandler >> takeKeystroke: anEvent in: aWorld [
	"Escape opens the world menu, when no morph has the keyboard focus"

	(anEvent isKeystroke and: [ anEvent keyCharacter asciiValue = 27 ]) ifFalse: [ ^ false ].
	anEvent hand keyboardFocus ifNotNil: [ ^ false ].
	self openWorldMenu: anEvent in: aWorld.
	^ true
]

{ #category : 'private' }
DesktopRootEventHandler >> takeMouseDown: evt in: aWorld [
	"A menu click opens the contents menu. A click opens the world menu, a drag draws a selection rectangle"

	(self isBackground: evt position of: aWorld) ifFalse: [ ^ false ].
	(aWorld class isMenuOpenByLeftClick
		 ifTrue: [ evt yellowButtonPressed ]
		 ifFalse: [ evt redButtonPressed ]) ifTrue: [
		self openContentsMenu: evt in: aWorld.
		^ true ].
	evt hand releaseKeyboardFocus.
	(evt shiftPressed or: [ aWorld isEasySelecting ]) ifFalse: [
		self openWorldMenu: evt in: aWorld.
		^ true ].
	evt hand
		waitForClicksOrDrag: self
		event: evt
		selectors: {
				(evt shiftPressed
					 ifTrue: [ #contentsMenuClick: ]
					 ifFalse: [ #worldMenuClick: ]).
				nil.
				nil.
				#selectionDrag: }
		threshold: 5.
	^ true
]

{ #category : 'private' }
DesktopRootEventHandler >> takeWindowEvent: anEvent in: aWorld [
	"Closing aWorld's window asks to quit, unless aWorld has its own window event handler"

	(anEvent type == #windowClose and: [ aWorld windowEventHandler isNil ]) ifFalse: [ ^ false ].
	self quitOnWindowClose: anEvent in: aWorld.
	^ true
]

{ #category : 'clicking' }
DesktopRootEventHandler >> worldMenuClick: evt [
	"Sent by the hand when the click on the background turned out to be a click"

	self openWorldMenu: evt in: evt hand world
]
