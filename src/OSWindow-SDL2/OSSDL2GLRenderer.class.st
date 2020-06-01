"
My instances implement OSWindowGLRenderer interface using SDL2 library
"
Class {
	#name : #OSSDL2GLRenderer,
	#superclass : #OSWindowGLRenderer,
	#instVars : [
		'context'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #'OSWindow-SDL2-OpenGL'
}

{ #category : #finalization }
OSSDL2GLRenderer class >> finalizeResourceData: windowAndContext [
	| window context |
	window := windowAndContext first.
	context := windowAndContext second.

	OSWindowRenderThread enqueueBlockingOperation: [
		SDL2
			glMakeCurrent: (SDL_Window null) context: (SDL_GLContext null).
		context delete
	].
]

{ #category : #misc }
OSSDL2GLRenderer >> checkThread [
OSWindowRenderThread isActiveThread ifFalse: [ ^ 'GL Renderer must be used inside the OSWindowRenderThread or a critical section' ]
]

{ #category : #misc }
OSSDL2GLRenderer >> createContext [
	self onRenderThreadBlocking: [
		context := SDL2 glCreateContext: backendWindow sdl2Window .
	].

	(context isNotNil and: [ context isNull not ]) ifTrue: [
		FFIExternalResourceManager addResource: self data: { backendWindow . context }
	]
]

{ #category : #initialize }
OSSDL2GLRenderer >> destroy [
	self onRenderThreadBlocking: [
		context ifNotNil: [ context delete ].
		context := nil.
	]
	
]

{ #category : #misc }
OSSDL2GLRenderer >> getProcAddress: procName [
	self checkThread.
	^ SDL2 glGetProcAddress: procName
]

{ #category : #misc }
OSSDL2GLRenderer >> makeCurrent: aBackendWindow [
	| windowHandle |
	context ifNil: [ ^ false ].

	self checkThread.
	windowHandle := aBackendWindow sdl2Window.
	windowHandle ifNil: [ ^ false ].
	^ (SDL2 glMakeCurrent: windowHandle context: context) == 0
]

{ #category : #misc }
OSSDL2GLRenderer >> swapBuffers: aWindow [
	self checkThread.
	SDL2 glSwapWindow: aWindow sdl2Window.
]

{ #category : #'updating screen' }
OSSDL2GLRenderer >> updateAll [
	"Do nothing here"

]

{ #category : #'updating screen' }
OSSDL2GLRenderer >> updateRectangle: rectangle [
	"Do nothing here"
]
