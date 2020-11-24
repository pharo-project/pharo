"
I am the default platform. All my methods are empty or have a default implementation
"
Class {
	#name : #SDLNullPlatform,
	#superclass : #SDLAbstractPlatform,
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #operations }
SDLNullPlatform >> afterSetWindowTitle: aString onWindow: aOSSDLWindow [
]

{ #category : #initialization }
SDLNullPlatform >> initPlatformSpecific [

	
]
