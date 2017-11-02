"
SDL2 library structure
"
Class {
	#name : #SDL2MappedEvent,
	#superclass : #SDL2Structure,
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #visitor }
SDL2MappedEvent class >> accept: aVisitor [
	self subclassResponsibility
]

{ #category : #'event type' }
SDL2MappedEvent class >> eventType [
	^ nil
]

{ #category : #'instance creation' }
SDL2MappedEvent class >> fromSdlEvent: event [
	^  self new setHandle: event getHandle
]

{ #category : #'event type' }
SDL2MappedEvent >> eventType [
	^ nil
]

{ #category : #accessing }
SDL2MappedEvent >> windowID [
	^ nil
]
