"
I am the root of the Calypso activations which run a command from a mouse event on a morph. I match the mouse button and the optional keyboard modifier of the event. Concrete subclasses declare the morphic event type to listen to, such as a click or a double click.
"
Class {
	#name : 'ClyMouseCommandActivation',
	#superclass : 'ClyCommandActivationStrategy',
	#instVars : [
		'whichButton',
		'keyboardModifier'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyMouseCommandActivation class >> byYellowButtonFor: aCommandUser [

	^(self for: aCommandUser)
		beYellowButton
]

{ #category : 'instance creation' }
ClyMouseCommandActivation class >> byYellowButtonWith: aKMModifier for: aCommandUser [

	^(self byYellowButtonFor: aCommandUser)
		keyboardModifier: aKMModifier
]

{ #category : 'morphs setup' }
ClyMouseCommandActivation class >> enableInMorph: aMorph withCommandsFrom: someObjectWithCommandContext [

	aMorph
		on: self morphicEventType
		send: #handleMouseUsing:event:from: to: self withValue: someObjectWithCommandContext
]

{ #category : 'command execition' }
ClyMouseCommandActivation class >> executeCommandInContext: aToolContext [

	self activateAllInContext: aToolContext by: [:activator |
		activator executeCommand.
		^true ].
	^false
]

{ #category : 'morphs setup' }
ClyMouseCommandActivation class >> handleMouseUsing: someObjectWithCommandContext event: anEvent from: sourceMorph [
	| targetContext |
	targetContext := someObjectWithCommandContext
		createCommandContextForCursorAt: anEvent cursorPoint.

	self executeCommandInContext: targetContext.

	anEvent wasHandled: false
]

{ #category : 'testing' }
ClyMouseCommandActivation class >> isAbstract [

	^ self == ClyMouseCommandActivation
]

{ #category : 'morphs setup' }
ClyMouseCommandActivation class >> morphicEventType [
	self subclassResponsibility
]

{ #category : 'instance creation' }
ClyMouseCommandActivation class >> with: aKMModifier for: aCommandUser [

	^(self for: aCommandUser)
		keyboardModifier: aKMModifier
]

{ #category : 'initialization' }
ClyMouseCommandActivation >> beBlueButton [

	whichButton := MouseEvent blueButton
]

{ #category : 'initialization' }
ClyMouseCommandActivation >> beRedButton [

	whichButton := MouseEvent redButton
]

{ #category : 'initialization' }
ClyMouseCommandActivation >> beYellowButton [

	whichButton := MouseEvent yellowButton
]

{ #category : 'initialization' }
ClyMouseCommandActivation >> initialize [
	super initialize.

	self beRedButton
]

{ #category : 'testing' }
ClyMouseCommandActivation >> isActiveInContext: aToolContext [
	| mouseEvent |
	mouseEvent := aToolContext currentWorld activeHand lastEvent.

	^(self matchesEvent: mouseEvent)
		and: [super isActiveInContext: aToolContext]
]

{ #category : 'testing' }
ClyMouseCommandActivation >> isBlueButton [

	^whichButton anyMask: MouseEvent blueButton
]

{ #category : 'testing' }
ClyMouseCommandActivation >> isRedButton [

	^whichButton anyMask: MouseEvent redButton
]

{ #category : 'testing' }
ClyMouseCommandActivation >> isYellowButton [

	^whichButton anyMask: MouseEvent yellowButton
]

{ #category : 'accessing' }
ClyMouseCommandActivation >> keyboardModifier [
	^ keyboardModifier
]

{ #category : 'accessing' }
ClyMouseCommandActivation >> keyboardModifier: anObject [
	keyboardModifier := anObject
]

{ #category : 'testing' }
ClyMouseCommandActivation >> matchesEvent: aMouseEvent [
	(aMouseEvent respondsTo: #whichButton) ifFalse: [ ^false ].
	aMouseEvent fixStateForClickAndModifierIssue.

	(keyboardModifier isNil and: [ "shift is somehow ignored by first check logic"
		aMouseEvent anyModifierKeyPressed | aMouseEvent shiftPressed]) ifTrue: [ ^false ].

	(keyboardModifier isNotNil and: [ (keyboardModifier matchesEvent: aMouseEvent) not ])
		ifTrue: [ ^false ].

	^aMouseEvent whichButton anyMask: whichButton
]

{ #category : 'printing' }
ClyMouseCommandActivation >> printOn: aStream [
	super printOn: aStream.

	aStream nextPut: $(.
	aStream nextPutAll: self printWhichButton.
	keyboardModifier ifNotNil: [
		aStream nextPutAll: ' '.
		keyboardModifier printOn: aStream ].
	aStream nextPut: $)
]

{ #category : 'printing' }
ClyMouseCommandActivation >> printWhichButton [
	"Return a string identifying expected pressed buttons"
	self isRedButton ifTrue:[ ^'red' ].
	self isYellowButton ifTrue:[ ^'yellow'].
	self isBlueButton ifTrue:[ 'blue'].

	^'uknown button'
]

{ #category : 'accessing' }
ClyMouseCommandActivation >> whichButton [
	^ whichButton
]

{ #category : 'accessing' }
ClyMouseCommandActivation >> whichButton: anObject [
	whichButton := anObject
]
