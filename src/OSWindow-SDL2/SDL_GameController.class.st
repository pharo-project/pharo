"
I represent a handler for a game controller coming from SDL2. 

A game controller is a joystick with an mapping. According to the SDL2 documentation, if a game es being executed via Steam, the game controller mapping is provided automatically.
"
Class {
	#name : #'SDL_GameController',
	#superclass : #SDL2Handle,
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #closing }
SDL_GameController >> close [
	^ self ffiCall: #( void SDL_GameControllerClose(self ) )
]

{ #category : #accessing }
SDL_GameController >> getAttached [
	^ self ffiCall: #( SDL_bool SDL_GameControllerGetAttached(self) )
]

{ #category : #accessing }
SDL_GameController >> getAxis: axis [
	^ self ffiCall: #( Sint16 SDL_GameControllerGetAxis(self , int axis) )
]

{ #category : #accessing }
SDL_GameController >> getButton: button [
	^ self ffiCall: #( Uint8 SDL_GameControllerGetButton(self , int button) )
]

{ #category : #testing }
SDL_GameController >> isGameController [
	^ true
]

{ #category : #accessing }
SDL_GameController >> name [
	^ self ffiCall: #( String SDL_GameControllerName(self) )
]
