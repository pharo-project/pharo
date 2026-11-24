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

{ #category : 'event handling' }
DesktopRootEventHandler >> handleContentsMenu: evt for: aWorld [
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

{ #category : 'event handling' }
DesktopRootEventHandler >> handleDragThrough: evt for: aWorld [
	"Draw out a selection rectangle"
	| selection |
	selection := SelectionMorph newBounds: (evt cursorPoint extent: 8@8).
	aWorld addMorph: selection.
	selection extendByHand: evt hand.
	^ true
]

{ #category : 'event handling' }
DesktopRootEventHandler >> handleWindowClose: anEvent for: aWorld [
	"The window close dialog is modal, since the windowClose event originates from the system window button, we need to protect against opening multiple, otherwise we end up with the world in a locked state when one of them is cancelled"

	(aWorld valueOfProperty: #canOpenCloseDialog ifAbsent: true) ifFalse: [ ^ true ].
	aWorld setProperty: #canOpenCloseDialog toValue: false.
	WorldState quitSession.
	aWorld removeProperty: #canOpenCloseDialog.
	^ true
]

{ #category : 'event handling' }
DesktopRootEventHandler >> handleWorldMenu: evt for: aWorld [
	"Put up the world menu, triggered by the passed-in event."

	"If the user does not want a world menu, do not invoke it."
	aWorld class shouldShowWorldMenu ifFalse: [ ^ false ].

	aWorld bringTopmostsToFront.
	"put up screen menu"
	aWorld worldMenu popUpEvent: evt in: aWorld.
	^ true
]
