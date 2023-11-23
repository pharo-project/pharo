"
I am delivered when window loses the user input focus.
"
Class {
	#name : 'OSWindowFocusOutEvent',
	#superclass : 'OSWindowEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSWindowFocusOutEvent >> accept: aVisitor [
	^ aVisitor visitWindowFocusOutEvent: self
]
