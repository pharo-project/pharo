"
i am delivered when user presses a mouse button
"
Class {
	#name : 'OSMouseButtonPressEvent',
	#superclass : 'OSMouseButtonEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSMouseButtonPressEvent >> accept: aVisitor [
	^ aVisitor visitMouseButtonPressEvent: self
]
