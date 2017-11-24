"
I am the root of the joystick button events hierarchy.

The which parameter identifies the joystick that produced this event.
The button parameter is the joystick dependent index of the button that was pressed or released.
The pressed parameter tells if the button was pressed or released.
The  mapped  parameters tells if the button index have been mapped into a standard controller layout.
The hasMapping parameters tells if this joystick is generating a mapped version of the event.
"
Class {
	#name : #OSJoyButtonEvent,
	#superclass : #OSEvent,
	#instVars : [
		'which',
		'button',
		'pressed',
		'mapped',
		'hasMapping'
	],
	#category : #'OSWindow-Core-Events'
}

{ #category : #accessing }
OSJoyButtonEvent >> button [
	
	^ button
]

{ #category : #accessing }
OSJoyButtonEvent >> button: anObject [
	
	button := anObject
]

{ #category : #accessing }
OSJoyButtonEvent >> hasMapping [
	^ hasMapping
]

{ #category : #accessing }
OSJoyButtonEvent >> hasMapping: anObject [
	hasMapping := anObject
]

{ #category : #testing }
OSJoyButtonEvent >> isButtonA [
	^ button == 0
]

{ #category : #testing }
OSJoyButtonEvent >> isButtonB [
	^ button == 1
]

{ #category : #testing }
OSJoyButtonEvent >> isButtonBack [
	^ button == 4
]

{ #category : #testing }
OSJoyButtonEvent >> isButtonGuide [
	^ button == 5
]

{ #category : #testing }
OSJoyButtonEvent >> isButtonStart [
	^ button == 6
]

{ #category : #testing }
OSJoyButtonEvent >> isButtonX [
	^ button == 2
]

{ #category : #testing }
OSJoyButtonEvent >> isButtonY [
	^ button == 3
]

{ #category : #testing }
OSJoyButtonEvent >> isDigitalPadDown [
	^ button == 12
]

{ #category : #testing }
OSJoyButtonEvent >> isDigitalPadLeft [
	^ button == 13
]

{ #category : #testing }
OSJoyButtonEvent >> isDigitalPadRight [
	^ button == 14
]

{ #category : #testing }
OSJoyButtonEvent >> isDigitalPadUp [
	^ button == 11
]

{ #category : #testing }
OSJoyButtonEvent >> isLeftShoulder [
	^ button == 9
]

{ #category : #testing }
OSJoyButtonEvent >> isLeftStick [
	^ button == 7
]

{ #category : #testing }
OSJoyButtonEvent >> isPreferredEvent [
	^ mapped or: hasMapping not
]

{ #category : #testing }
OSJoyButtonEvent >> isRightShoulder [
	^ button == 10
]

{ #category : #testing }
OSJoyButtonEvent >> isRightStick [
	^ button == 8
]

{ #category : #accessing }
OSJoyButtonEvent >> mapped [
	^ mapped
]

{ #category : #accessing }
OSJoyButtonEvent >> mapped: anObject [
	mapped := anObject
]

{ #category : #accessing }
OSJoyButtonEvent >> pressed [
	
	^ pressed
]

{ #category : #accessing }
OSJoyButtonEvent >> pressed: anObject [
	
	pressed := anObject
]

{ #category : #accessing }
OSJoyButtonEvent >> which [
	
	^ which
]

{ #category : #accessing }
OSJoyButtonEvent >> which: anObject [
	
	which := anObject
]
