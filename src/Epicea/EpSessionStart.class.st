"
When an image is launched, a session starts. I represent the start of a session.
"
Class {
	#name : #EpSessionStart,
	#superclass : #EpSessionEvent,
	#category : #'Epicea-Model'
}

{ #category : #visitor }
EpSessionStart >> accept: aVisitor [ 
	^ aVisitor visitSessionStart: self
]
