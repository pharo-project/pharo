"
SDL2 library structure
"
Class {
	#name : #SDL_QuitEvent,
	#superclass : #SDL_CommonEvent,
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'event type' }
SDL_QuitEvent class >> eventType [
	^ SDL_QUIT
]

{ #category : #visitor }
SDL_QuitEvent >> accept: aVisitor [
	^ aVisitor visitQuitEvent: self
]

{ #category : #'accessing structure variables' }
SDL_QuitEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_QuitEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_QuitEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_QuitEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]
