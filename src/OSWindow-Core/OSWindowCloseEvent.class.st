"
I am delivered when user closing the window.
The default action (unless suppressed) will also destroy the window.
"
Class {
	#name : #OSWindowCloseEvent,
	#superclass : #OSWindowEvent,
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSWindowCloseEvent >> accept: aVisitor [
	^ aVisitor visitWindowCloseEvent: self
]

{ #category : #'default actions' }
OSWindowCloseEvent >> defaultAction [
	window destroy
]
