"
I am delivered , when driver don't knows how to interpret the event which comes from OS.
Useful for debugging.
"
Class {
	#name : #OSUnknownEvent,
	#superclass : #OSEvent,
	#instVars : [
		'data'
	],
	#category : 'OSWindow-Core-Events'
}

{ #category : #visitor }
OSUnknownEvent >> accept: aVisitor [
	^ aVisitor visitUnknownEvent: self
]

{ #category : #accessing }
OSUnknownEvent >> data [
	
	^ data
]

{ #category : #accessing }
OSUnknownEvent >> data: anObject [
	
	data := anObject
]
