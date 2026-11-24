"
I check that a desktop world created programmatically is a desktop: it has the desktop's root event handlers.
"
Class {
	#name : 'DesktopWorldMorphTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Desktop-Tests',
	#package : 'Morphic-Desktop-Tests'
}

{ #category : 'helpers' }
DesktopWorldMorphTest >> altTabWith: aHand [

	^ (KeyboardEvent new
		   setType: #keyDown
		   buttons: (UserInputEvent classVarNamed: #ALT_KEY_MASK)
		   position: 0 @ 0
		   keyValue: Character tab asInteger
		   charCode: Character tab asInteger
		   hand: aHand
		   stamp: 0)
		  key: Character tab asKeyCombination key;
		  yourself
]

{ #category : 'tests' }
DesktopWorldMorphTest >> testAPlainWorldIsNoDesktop [

	self assertEmpty: WorldMorph new rootEventHandlers
]

{ #category : 'tests' }
DesktopWorldMorphTest >> testAltTabOpensTheTaskListOfTheDesktop [

	| desktop hand |
	desktop := DesktopWorldMorph new.
	desktop extent: 800 @ 600.
	desktop
		addMorph: (SystemWindow labelled: 'first');
		addMorph: (SystemWindow labelled: 'second').
	hand := HandMorph new.
	desktop addHand: hand.

	self assert: (desktop rootEventHandlersTake: (self altTabWith: hand)).

	self assert: (desktop submorphs anySatisfy: [ :morph | morph isKindOf: TaskListMorph ])
]

{ #category : 'tests' }
DesktopWorldMorphTest >> testHasTheDesktopHandlers [

	self assert: (DesktopWorldMorph new rootEventHandlers collect: [ :handler | handler class ]) asArray equals: {
		DesktopRootEventHandler.
		TaskbarRootEventHandler.
		HaloRootEventHandler }
]

{ #category : 'tests' }
DesktopWorldMorphTest >> testNotifiesTheUserInItself [

	| desktop |
	desktop := DesktopWorldMorph new.
	desktop extent: 800 @ 600.

	desktop notifyUser: 'hello'.

	self assert: (desktop submorphs anySatisfy: [ :morph | morph isKindOf: GrowlMorph ])
]

{ #category : 'tests' }
DesktopWorldMorphTest >> testTakesTheDesktopColorOfItsTheme [

	| world theme |
	world := DesktopWorldMorph new.
	world color: Color red.
	theme := UITheme current class new.
	world theme: theme.
	self assert: world color equals: theme desktopColor
]
