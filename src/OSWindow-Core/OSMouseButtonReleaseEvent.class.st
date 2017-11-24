"
i am delivered when user releases a mouse button
"
Class {
	#name : #OSMouseButtonReleaseEvent,
	#superclass : #OSMouseButtonEvent,
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSMouseButtonReleaseEvent >> accept: aVisitor [
	^ aVisitor visitMouseButtonReleaseEvent: self
]
