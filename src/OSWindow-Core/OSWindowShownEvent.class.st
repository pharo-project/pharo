"
I am delivered when a window is shown.
"
Class {
	#name : #OSWindowShownEvent,
	#superclass : #OSWindowEvent,
	#instVars : [
		'timestamp'
	],
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSWindowShownEvent >> accept: aVisitor [
	^ aVisitor visitWindowShownEvent: self
]

{ #category : #visitor }
OSWindowShownEvent >> defaultAction [
	window isValid ifTrue: [ window renderer ifNotNil: [ window renderer exposed: timestamp  ] ]
]

{ #category : #accessing }
OSWindowShownEvent >> timestamp [
	^ timestamp
]

{ #category : #accessing }
OSWindowShownEvent >> timestamp: anObject [
	timestamp := anObject
]
