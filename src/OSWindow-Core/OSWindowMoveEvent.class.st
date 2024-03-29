"
I am delivered when window moved by user or OS/window manager.
"
Class {
	#name : 'OSWindowMoveEvent',
	#superclass : 'OSWindowEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSWindowMoveEvent >> accept: aVisitor [
	^ aVisitor visitWindowMoveEvent: self
]
