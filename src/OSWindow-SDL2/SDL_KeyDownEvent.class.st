"
SDL2 library structure
"
Class {
	#name : 'SDL_KeyDownEvent',
	#superclass : 'SDL_KeyboardEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_KeyDownEvent class >> eventType [
	^ SDL_KEYDOWN
]

{ #category : 'visitor' }
SDL_KeyDownEvent >> accept: aVisitor [
	^ aVisitor visitKeyDownEvent: self
]

{ #category : 'testing' }
SDL_KeyDownEvent >> isKeyDownEvent [

	^ true
]

{ #category : 'testing' }
SDL_KeyDownEvent >> isUserInterrupt [

	^ ((self keysym mod bitAnd: 16r200) = 0) "ignore all if AltGr is pressed"
		and: [(self keysym mod anyMask: 1344) "This mask is for meta/cmd"
			and: [ self keysym sym = $. asInteger ] ]
]
