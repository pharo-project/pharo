"
I represent a switch that can be either on or off.  I chnage my state in response to a mouse click.  When clicked, I also send my actionSelector to my target, just like a SimpleButtonMorph.

	SimpleSwitchMorph new openInWorld
"
Class {
	#name : #SimpleSwitchMorph,
	#superclass : #SimpleButtonMorph,
	#instVars : [
		'onColor',
		'offColor'
	],
	#category : #Morphic-Widgets-Basic-Buttons
}

{ #category : #button }
SimpleSwitchMorph >> doButtonAction [
	"Perform the action of this button. The last argument of the message sent to the target is the new state of this switch."

	| newState |
	(target notNil and: [actionSelector notNil]) 
		ifTrue: 
			[newState := color = onColor.
			target perform: actionSelector
				withArguments: (arguments copyWith: newState)]
]

{ #category : #initialization }
SimpleSwitchMorph >> initialize [

	super initialize.
	self borderWidth: 3.
	self extent: self extent + 2.
	onColor := Color r: 1.0 g: 0.6 b: 0.6.
	offColor := Color lightGray.
	color := offColor

]

{ #category : #switching }
SimpleSwitchMorph >> isOn [
	^ color = onColor
]

{ #category : #'event handling' }
SimpleSwitchMorph >> mouseDown: evt [

	oldColor := self fillStyle.
]

{ #category : #'event handling' }
SimpleSwitchMorph >> mouseMove: evt [

	(self containsPoint: evt cursorPoint)
		ifTrue: [self setSwitchState: (oldColor = offColor)]
		ifFalse: [self setSwitchState: (oldColor = onColor)].

]

{ #category : #'event handling' }
SimpleSwitchMorph >> mouseUp: evt [

	(self containsPoint: evt cursorPoint)
		ifTrue: [  "toggle and do action"
			self setSwitchState: (oldColor = offColor).
			self doButtonAction]
		ifFalse: [  "restore old appearance"
			self setSwitchState: (oldColor = onColor)].

]

{ #category : #switching }
SimpleSwitchMorph >> offColor: aColor [

	offColor := aColor.

]

{ #category : #switching }
SimpleSwitchMorph >> onColor: aColor [

	onColor := aColor.

]

{ #category : #switching }
SimpleSwitchMorph >> setSwitchState: aBoolean [

	aBoolean
		ifTrue: [self turnOn]
		ifFalse: [self turnOff].

]

{ #category : #switching }
SimpleSwitchMorph >> toggleState [
	self isOn
		ifTrue: [self turnOff]
		ifFalse: [self turnOn]
]

{ #category : #switching }
SimpleSwitchMorph >> turnOff [
	self borderColor: #raised.
	self color: offColor
]

{ #category : #switching }
SimpleSwitchMorph >> turnOn [
	self borderColor: #inset.
	self color: onColor
]
