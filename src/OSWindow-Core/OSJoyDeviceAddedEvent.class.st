"
I am a joystick device added event.

This event is generated when a joystick is connected to the computer.
"
Class {
	#name : #OSJoyDeviceAddedEvent,
	#superclass : #OSJoyDeviceEvent,
	#category : #OSWindow-Core-Events
}

{ #category : #visitor }
OSJoyDeviceAddedEvent >> accept: aVisitor [
	^ aVisitor visitJoyDeviceAddedEvent: self
]
