"
I am delivered when a window is hidden.
"
Class {
	#name : #OSWindowHiddenEvent,
	#superclass : #OSWindowEvent,
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSWindowHiddenEvent >> accept: aVisitor [
	^ aVisitor visitWindowHiddenEvent: self
]
