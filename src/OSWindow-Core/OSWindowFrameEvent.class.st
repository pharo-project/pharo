"
Some strange event nobody knows what it does.??
"
Class {
	#name : #OSWindowFrameEvent,
	#superclass : #OSWindowEvent,
	#category : #OSWindow-Core-Events
}

{ #category : #visitor }
OSWindowFrameEvent >> accept: aVisitor [
	^ aVisitor visitWindowFrameEvent: self
]
