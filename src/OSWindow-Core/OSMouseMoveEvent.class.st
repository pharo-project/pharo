"
i am delivered when user moves a mouse
"
Class {
	#name : #OSMouseMoveEvent,
	#superclass : #OSMouseEvent,
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSMouseMoveEvent >> accept: aVisitor [
	^ aVisitor visitMouseMoveEvent: self
]
