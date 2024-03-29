"
This event is delivered when user pressing down a joystick button.
"
Class {
	#name : 'OSJoyButtonDownEvent',
	#superclass : 'OSJoyButtonEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSJoyButtonDownEvent >> accept: aVisitor [
	^ aVisitor visitJoyButtonDownEvent: self
]
