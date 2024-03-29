"
I am delivered when mouse leaves the area , covered by a window
"
Class {
	#name : 'OSWindowMouseLeaveEvent',
	#superclass : 'OSWindowEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSWindowMouseLeaveEvent >> accept: aVisitor [
	^ aVisitor visitWindowMouseLeaveEvent: self
]
