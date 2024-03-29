"
I am delivered when the window got minimized.
"
Class {
	#name : 'OSWindowMinimizedEvent',
	#superclass : 'OSWindowEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSWindowMinimizedEvent >> accept: aVisitor [
	^ aVisitor visitWindowMinimizedEvent: self
]
