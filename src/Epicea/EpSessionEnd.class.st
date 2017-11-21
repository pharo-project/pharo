"
When an image is launched, a session starts. When it is quit, the session ends. I represent the end of a session.
"
Class {
	#name : #EpSessionEnd,
	#superclass : #EpSessionEvent,
	#category : #Epicea-Model
}

{ #category : #visitor }
EpSessionEnd >> accept: aVisitor [ 
	^ aVisitor visitSessionEnd: self
]
