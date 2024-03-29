"
I am delivered when mouse enters the area , covered by a window
"
Class {
	#name : 'OSWindowMouseEnterEvent',
	#superclass : 'OSWindowEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSWindowMouseEnterEvent >> accept: aVisitor [
	^ aVisitor visitWindowMouseEnterEvent: self
]
