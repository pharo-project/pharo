Class {
	#name : #'SDL_EventTest',
	#superclass : #TestCase,
	#category : #'OSWindow-SDL2-Tests'
}

{ #category : #tests }
SDL_EventTest >> testMapEvent [
	self assert: SDL_Event exampleKeyDownEvent mapped class equals: SDL_KeyDownEvent.
]

{ #category : #tests }
SDL_EventTest >> testMapUnknownEvent [
	self assert: SDL_Event exampleUnknownEvent mapped class equals: SDL_CommonEvent
]
