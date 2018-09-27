"
I represent a handler for a joystick coming from SDL2.
"
Class {
	#name : #'SDL_Joystick',
	#superclass : #SDL2Handle,
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #closing }
SDL_Joystick >> close [
	^ self ffiCall: #( void SDL_JoystickClose(self ) )
]

{ #category : #accessing }
SDL_Joystick >> getAttached [
	^ self ffiCall: #( SDL_bool SDL_JoystickGetAttached(self) )
]

{ #category : #accessing }
SDL_Joystick >> getAxis: axis [
	^ self ffiCall: #( Sint16 SDL_JoystickGetAxis(self , int axis) )
]

{ #category : #accessing }
SDL_Joystick >> getButton: button [
	^ self ffiCall: #( Sint16 SDL_JoystickGetButton(self , int button) )
]

{ #category : #testing }
SDL_Joystick >> isGameController [
	^ false
]

{ #category : #accessing }
SDL_Joystick >> name [
	^ self ffiCall: #( String SDL_JoystickName(self) )
]

{ #category : #accessing }
SDL_Joystick >> numberOfAxes [
	^ self ffiCall: #( int SDL_JoystickNumAxes(self) )
]

{ #category : #accessing }
SDL_Joystick >> numberOfBalls [
	^ self ffiCall: #( int SDL_JoystickNumBalls(self) )
]

{ #category : #accessing }
SDL_Joystick >> numberOfButtons [
	^ self ffiCall: #( int SDL_JoystickNumButtons(self) )
]

{ #category : #accessing }
SDL_Joystick >> numberOfHats [
	^ self ffiCall: #( int SDL_JoystickNumHats(self) )
]
