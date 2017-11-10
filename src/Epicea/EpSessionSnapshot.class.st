"
When an image is launched, a session starts. After that, the session can be snapshoted multiple times. I represent the snapshot of a session.
"
Class {
	#name : #EpSessionSnapshot,
	#superclass : #EpSessionEvent,
	#category : #Epicea-Model
}

{ #category : #visitor }
EpSessionSnapshot >> accept: aVisitor [

	^ aVisitor visitSessionSnapshot: self
]
