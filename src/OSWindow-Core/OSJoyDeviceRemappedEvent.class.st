"
I am a joystick device remapped event.

This event is generated when a joystick layout is remapped.
"
Class {
	#name : #OSJoyDeviceRemappedEvent,
	#superclass : #OSJoyDeviceEvent,
	#category : 'OSWindow-Core-Events'
}

{ #category : #visitor }
OSJoyDeviceRemappedEvent >> accept: aVisitor [
	^ aVisitor visitJoyDeviceRemappedEvent: self
]
