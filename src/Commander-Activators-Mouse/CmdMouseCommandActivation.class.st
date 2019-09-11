"
My subclasses represent particular kind of mouse events which should activate annotated commands.
For example it can be mouse click or double click.

My instances are configured with type of mouse button and keyboard modifier which are expected to be used to activate commands.

By default the left click is expected:

	CmdClickActivation for: aCommandUser.

With extra parameter you can configure modifier: 

	CmdClickActivation with: KMModifier shift for: aCommandUser.
	
To specify mouse button use following messages: 

	(CmdClickActivation for: aCommandUser) beBlueButton.
	(CmdClickActivation for: aCommandUser) beYellowButton.
	(CmdClickActivation for: aCommandUser) beRedButton

And there is special constructor for yellow button which is usefull for various context menu activations:

	CmdClickActivation byYellowButtonFor: aCommandUser.
	CmdClickActivation byYellowButtonWith: KMModifier shift for: aCommandUser

My instances are active when they are match last mouse event:

	aMouseActivation isActiveInContext: aToolContext 
	
I extend this method to check that last mouse event matches expected button and modifier. 
		
There are few methods how to enable mouse commands in the morphs: 

	aMorph enableMouseCommands: CmdClickActivation withContextFrom: aToolContext.

It enables click action to execute commands in given aMorph instance.

	aMorph enableAllMouseCommandsFrom: aToolContext.    	 
		
It enables all kind of mouse events to execute commands in given aMorph instance.

Internal Representation and Key Implementation Points.

    Instance Variables
	keyboardModifier:		<KMModifier>
	whichButton:		<Integer>
"
Class {
	#name : #CmdMouseCommandActivation,
	#superclass : #CmdCommandActivationStrategy,
	#instVars : [
		'whichButton',
		'keyboardModifier'
	],
	#category : #'Commander-Activators-Mouse'
}

{ #category : #'instance creation' }
CmdMouseCommandActivation class >> byYellowButtonFor: aCommandUser [

	^(self for: aCommandUser)
		beYellowButton
]

{ #category : #'instance creation' }
CmdMouseCommandActivation class >> byYellowButtonWith: aKMModifier for: aCommandUser [

	^(self byYellowButtonFor: aCommandUser)
		keyboardModifier: aKMModifier
]

{ #category : #'morphs setup' }
CmdMouseCommandActivation class >> enableInMorph: aMorph withCommandsFrom: someObjectWithCommandContext [

	aMorph 
		on: self morphicEventType 
		send: #handleMouseUsing:event:from: to: self withValue: someObjectWithCommandContext
]

{ #category : #'command execition' }
CmdMouseCommandActivation class >> executeCommandInContext: aToolContext [

	self activateAllInContext: aToolContext by: [:activator | 
		activator executeCommand.
		^true ].
	^false
]

{ #category : #'morphs setup' }
CmdMouseCommandActivation class >> handleMouseUsing: someObjectWithCommandContext event: anEvent from: sourceMorph [ 
	| targetContext |
	targetContext := someObjectWithCommandContext 
		createCommandContextForCursorAt: anEvent cursorPoint.
	
	self executeCommandInContext: targetContext.
	
	anEvent wasHandled: false
]

{ #category : #'morphs setup' }
CmdMouseCommandActivation class >> morphicEventType [
	self subclassResponsibility 
]

{ #category : #'instance creation' }
CmdMouseCommandActivation class >> with: aKMModifier for: aCommandUser [

	^(self for: aCommandUser)
		keyboardModifier: aKMModifier
]

{ #category : #initialization }
CmdMouseCommandActivation >> beBlueButton [
	
	whichButton := MouseEvent blueButton
]

{ #category : #initialization }
CmdMouseCommandActivation >> beRedButton [
	
	whichButton := MouseEvent redButton
]

{ #category : #initialization }
CmdMouseCommandActivation >> beYellowButton [
	
	whichButton := MouseEvent yellowButton
]

{ #category : #initialization }
CmdMouseCommandActivation >> initialize [
	super initialize.
	
	self beRedButton 
]

{ #category : #testing }
CmdMouseCommandActivation >> isActiveInContext: aToolContext [
	| mouseEvent |
	mouseEvent := ActiveHand lastEvent.
	
	^(self matchesEvent: mouseEvent)
		and: [super isActiveInContext: aToolContext]
]

{ #category : #testing }
CmdMouseCommandActivation >> isBlueButton [

	^whichButton anyMask: MouseEvent blueButton
]

{ #category : #testing }
CmdMouseCommandActivation >> isRedButton [

	^whichButton anyMask: MouseEvent redButton
]

{ #category : #testing }
CmdMouseCommandActivation >> isYellowButton [

	^whichButton anyMask: MouseEvent yellowButton
]

{ #category : #accessing }
CmdMouseCommandActivation >> keyboardModifier [
	^ keyboardModifier
]

{ #category : #accessing }
CmdMouseCommandActivation >> keyboardModifier: anObject [
	keyboardModifier := anObject
]

{ #category : #testing }
CmdMouseCommandActivation >> matchesEvent: aMouseEvent [
	(aMouseEvent respondsTo: #whichButton) ifFalse: [ ^false ].
	aMouseEvent fixStateForClickAndModifierIssue.
	
	(keyboardModifier isNil and: [ "shift is somehow ignored by first check logic"
		aMouseEvent anyModifierKeyPressed | aMouseEvent shiftPressed]) ifTrue: [ ^false ].
	
	(keyboardModifier notNil and: [ (keyboardModifier matchesEvent: aMouseEvent) not ])
		ifTrue: [ ^false ].
	
	^aMouseEvent whichButton anyMask: whichButton
]

{ #category : #printing }
CmdMouseCommandActivation >> printOn: aStream [
	super printOn: aStream.
	
	aStream nextPut: $(.
	aStream nextPutAll: self printWhichButton.
	keyboardModifier ifNotNil: [ 
		aStream nextPutAll: ' '.
		keyboardModifier printOn: aStream ].
	aStream nextPut: $).
]

{ #category : #printing }
CmdMouseCommandActivation >> printWhichButton [
	"Return a string identifying expected pressed buttons"
	self isRedButton ifTrue:[ ^'red' ].
	self isYellowButton ifTrue:[ ^'yellow'].
	self isBlueButton ifTrue:[ 'blue'].
	
	^'uknown button'
]

{ #category : #accessing }
CmdMouseCommandActivation >> whichButton [
	^ whichButton
]

{ #category : #accessing }
CmdMouseCommandActivation >> whichButton: anObject [
	whichButton := anObject
]
