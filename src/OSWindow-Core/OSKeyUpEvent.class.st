"
This event is delivered when user releases previously pressed key
"
Class {
	#name : 'OSKeyUpEvent',
	#superclass : 'OSKeyboardEvent',
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSKeyUpEvent >> accept: aVisitor [
	^ aVisitor visitKeyUpEvent: self
]
