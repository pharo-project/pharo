"
This event is delivered when user releases a previously pressed joystick button.
"
Class {
	#name : #OSJoyButtonUpEvent,
	#superclass : #OSJoyButtonEvent,
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSJoyButtonUpEvent >> accept: aVisitor [
	^ aVisitor visitJoyButtonUpEvent: self
]
