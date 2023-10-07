"
This event is delivered when user pressing down a key
"
Class {
	#name : 'OSKeyDownEvent',
	#superclass : 'OSKeyboardEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSKeyDownEvent >> accept: aVisitor [
	^ aVisitor visitKeyDownEvent: self
]
