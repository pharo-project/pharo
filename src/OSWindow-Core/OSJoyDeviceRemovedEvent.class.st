"
I am a joystick device added event.

This event is generated when a joystick is disconnected from the computer.
"
Class {
	#name : #OSJoyDeviceRemovedEvent,
	#superclass : #OSJoyDeviceEvent,
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSJoyDeviceRemovedEvent >> accept: aVisitor [
	^ aVisitor visitJoyDeviceRemovedEvent: self
]
