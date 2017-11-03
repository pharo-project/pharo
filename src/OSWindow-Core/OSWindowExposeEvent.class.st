"
I am delivered when part (or whole) window are exposed on the desktop and needs refreshing.
"
Class {
	#name : #OSWindowExposeEvent,
	#superclass : #OSWindowEvent,
	#instVars : [
		'timestamp'
	],
	#category : #OSWindow-Core-Events
}

{ #category : #visitor }
OSWindowExposeEvent >> accept: aVisitor [
	^ aVisitor visitWindowExposeEvent: self
]

{ #category : #'default actions' }
OSWindowExposeEvent >> defaultAction [
	window renderer ifNotNil: [ window renderer exposed: timestamp ]
]

{ #category : #accessing }
OSWindowExposeEvent >> timestamp [
	^ timestamp
]

{ #category : #accessing }
OSWindowExposeEvent >> timestamp: anObject [
	timestamp := anObject
]
