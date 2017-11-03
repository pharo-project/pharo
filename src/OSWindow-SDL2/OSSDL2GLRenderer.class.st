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
	#category : #OSWindow-SDL2-OpenGL
}

{ #category : #misc }
OSSDL2GLRenderer >> checkThread [
	OSWindowRenderThread isThisThread ifFalse: [ ^ 'GL Renderer must be used inside the OSWindowRenderThread' ]
]

{ #category : #misc }
OSSDL2GLRenderer >> createContext [
	self onRenderThreadBlocking: [
		context := SDL2 glCreateContext: window handle.
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
OSSDL2GLRenderer >> makeCurrent: aWindow [
	| windowHandle |
	context ifNil: [ ^ false ].
	
	self checkThread.
	windowHandle := aWindow handle.
	windowHandle ifNil: [ ^ false ].
	(SDL2 glMakeCurrent: windowHandle context: context) == 0
		ifTrue:  [ ^ true ]
		ifFalse:  [ ^ false ]
	
]

{ #category : #misc }
OSSDL2GLRenderer >> swapBuffers: aWindow [
	self checkThread.
	SDL2 glSwapWindow: aWindow handle.
]

{ #category : #'updating screen' }
OSSDL2GLRenderer >> updateAll [
	"Do nothing here"

]

{ #category : #'updating screen' }
OSSDL2GLRenderer >> updateRectangle: rectangle [
	"Do nothing here"
]
