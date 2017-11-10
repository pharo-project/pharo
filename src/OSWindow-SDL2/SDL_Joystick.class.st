"
I represent a handler for a joystick coming from SDL2.
"
Class {
	#name : #SDL_Joystick,
	#superclass : #SDL2Handle,
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #closing }
SDL_Joystick >> close [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_JoystickClose(self ) )
]

{ #category : #accessing }
SDL_Joystick >> getAttached [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_bool SDL_JoystickGetAttached(self) )
]

{ #category : #accessing }
SDL_Joystick >> getAxis: axis [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Sint16 SDL_JoystickGetAxis(self , int axis) )
]

{ #category : #accessing }
SDL_Joystick >> getButton: button [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Sint16 SDL_JoystickGetButton(self , int button) )
]

{ #category : #testing }
SDL_Joystick >> isGameController [
	^ false
]

{ #category : #accessing }
SDL_Joystick >> name [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( String SDL_JoystickName(self) )
]

{ #category : #accessing }
SDL_Joystick >> numberOfAxes [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_JoystickNumAxes(self) )
]

{ #category : #accessing }
SDL_Joystick >> numberOfBalls [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_JoystickNumBalls(self) )
]

{ #category : #accessing }
SDL_Joystick >> numberOfButtons [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_JoystickNumButtons(self) )
]

{ #category : #accessing }
SDL_Joystick >> numberOfHats [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_JoystickNumHats(self) )
]
