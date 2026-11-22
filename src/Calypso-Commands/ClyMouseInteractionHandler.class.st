"
I run the commands of a mouse interaction of ClyCommandMap (a click or a double click) when a morph receives the mouse event.
The command provider creates the tool context at the cursor (#createCommandContextForCursorAt:), and the first command of the interaction that can run in it is run, see ClyCommandRegistry>>#executeFirstCommandOf:inContext:.
For example the method editor runs the commands declared for the clicks in its left bar:
	ClyMouseInteractionHandler enableClick: #textLeftBarClick on: self leftSideBar withCommandsFrom: self
"
Class {
	#name : 'ClyMouseInteractionHandler',
	#superclass : 'Object',
	#instVars : [
		'interactionName',
		'commandProvider'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'enabling' }
ClyMouseInteractionHandler class >> enable: anInteractionName on: aMorph event: aMorphicEventType withCommandsFrom: aCommandProvider [

	| handler |
	handler := self new setInteractionName: anInteractionName commandProvider: aCommandProvider.
	aMorph on: aMorphicEventType send: #handleEvent:from: to: handler.
	^ handler
]

{ #category : 'enabling' }
ClyMouseInteractionHandler class >> enableClick: anInteractionName on: aMorph withCommandsFrom: aCommandProvider [
	"Morphic does not generate a click for the right (yellow) button, so the mouse up of this button is handled too."

	| handler |
	handler := self
		           enable: anInteractionName
		           on: aMorph
		           event: #click
		           withCommandsFrom: aCommandProvider.
	aMorph on: #mouseUp send: #handleYellowMouseUp:from: to: handler.
	^ handler
]

{ #category : 'event handling' }
ClyMouseInteractionHandler >> handleEvent: aMouseEvent from: aMorph [

	ClyCommandRegistry default executeFirstCommandOf: interactionName inContext: (commandProvider createCommandContextForCursorAt: aMouseEvent cursorPoint).
	aMouseEvent wasHandled: false
]

{ #category : 'event handling' }
ClyMouseInteractionHandler >> handleYellowMouseUp: aMouseEvent from: aMorph [

	aMouseEvent yellowButtonChanged ifTrue: [ self handleEvent: aMouseEvent from: aMorph ]
]

{ #category : 'initialization' }
ClyMouseInteractionHandler >> setInteractionName: aSymbol commandProvider: anObject [

	interactionName := aSymbol.
	commandProvider := anObject
]
