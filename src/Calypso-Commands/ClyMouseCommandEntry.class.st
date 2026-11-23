"
I am a command declared for a click or a double click in a Calypso command map, with the mouse button and the optional keyboard modifier it waits for:
	aCommandMap doubleClick add: ClySpawnFullBrowserCommand.
	(aCommandMap textLeftBarClick add: ClyOpenTextLeftBarMenuCommand) beYellowButton
I am active only when the last mouse event is a click of my button with my keyboard modifier.
"
Class {
	#name : 'ClyMouseCommandEntry',
	#superclass : 'ClyCommandEntry',
	#instVars : [
		'mouseButton',
		'keyboardModifier'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'initialization' }
ClyMouseCommandEntry >> beBlueButton [

	mouseButton := #blue
]

{ #category : 'initialization' }
ClyMouseCommandEntry >> beYellowButton [

	mouseButton := #yellow
]

{ #category : 'activation' }
ClyMouseCommandEntry >> isActiveInContext: aToolContext [

	^ (self matchesEvent: aToolContext currentWorld activeHand lastEvent) and: [ super isActiveInContext: aToolContext ]
]

{ #category : 'accessing' }
ClyMouseCommandEntry >> keyboardModifier [

	^ keyboardModifier
]

{ #category : 'accessing' }
ClyMouseCommandEntry >> keyboardModifier: aKMModifier [

	keyboardModifier := aKMModifier
]

{ #category : 'activation' }
ClyMouseCommandEntry >> matchesEvent: aMouseEvent [
	"Whether the event is a click of my button with my keyboard modifier, or with no modifier when I have none."

	aMouseEvent isMouse ifFalse: [ ^ false ].
	aMouseEvent fixStateForClickAndModifierIssue.
	(keyboardModifier isNil and: [ aMouseEvent anyModifierKeyPressed | aMouseEvent shiftPressed ]) ifTrue: [ ^ false ].
	(keyboardModifier isNotNil and: [ (keyboardModifier matchesEvent: aMouseEvent) not ]) ifTrue: [ ^ false ].
	^ aMouseEvent whichButton anyMask: self whichButton
]

{ #category : 'accessing' }
ClyMouseCommandEntry >> mouseButton [
	"#red (the default), #yellow or #blue."

	^ mouseButton ifNil: [ #red ]
]

{ #category : 'printing' }
ClyMouseCommandEntry >> printWhichButton [

	^ self mouseButton asString
]

{ #category : 'activation' }
ClyMouseCommandEntry >> whichButton [

	self mouseButton = #yellow ifTrue: [ ^ MouseEvent yellowButton ].
	self mouseButton = #blue ifTrue: [ ^ MouseEvent blueButton ].
	^ MouseEvent redButton
]
