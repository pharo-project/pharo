"
This event is delivered when user pressing down a key
"
Class {
	#name : #OSKeyDownEvent,
	#superclass : #OSKeyboardEvent,
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSKeyDownEvent >> accept: aVisitor [
	^ aVisitor visitKeyDownEvent: self
]
