"
I am delivered when the window got maximized.
"
Class {
	#name : 'OSWindowMaximizedEvent',
	#superclass : 'OSWindowEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSWindowMaximizedEvent >> accept: aVisitor [
	^ aVisitor visitWindowMaximizedEvent: self
]
