"
I am delivered when window obtains a user input focus.
"
Class {
	#name : #OSWindowFocusInEvent,
	#superclass : #OSWindowEvent,
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSWindowFocusInEvent >> accept: aVisitor [
	^ aVisitor visitWindowFocusInEvent: self
]
