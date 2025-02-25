"
I am a simple class implementing some features of SDL that are platform specific.
Get the corresponding class to use through

 OSPlatform current sdlPlatform
"
Class {
	#name : 'SDLAbstractPlatform',
	#superclass : 'Object',
	#pools : [
		'SDL2Constants',
		'SDL2ConstantsHint'
	],
	#category : 'OSWindow-SDL2-Platforms',
	#package : 'OSWindow-SDL2',
	#tag : 'Platforms'
}

{ #category : 'operations' }
SDLAbstractPlatform >> afterMainPharoWindowCreated: aOSSDLWindow [

	self subclassResponsibility
]

{ #category : 'operations' }
SDLAbstractPlatform >> afterSetWindowTitle: aString onWindow: aOSSDLWindow [

	self subclassResponsibility
]

{ #category : 'configuration' }
SDLAbstractPlatform >> defaultRendererFlags [
	"We have no special flags for the renderer.
	SDL_RENDERER_PRESENTVSYNC was shown to crash on certain windows machines with external screens"
	^ 0
]

{ #category : 'initialization' }
SDLAbstractPlatform >> initPlatformSpecific [

	self subclassResponsibility
]

{ #category : 'initialization' }
SDLAbstractPlatform >> systemCursorConversionTable [

	^ #()
]
