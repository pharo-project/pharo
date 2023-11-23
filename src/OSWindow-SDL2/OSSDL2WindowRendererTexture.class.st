"
I am a handle for a SDL2 texture created by an OSSDL2GenericRenderer.
"
Class {
	#name : 'OSSDL2WindowRendererTexture',
	#superclass : 'OSWindowRendererTexture',
	#instVars : [
		'extent',
		'handle'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : 'OSWindow-SDL2-Rendering',
	#package : 'OSWindow-SDL2',
	#tag : 'Rendering'
}

{ #category : 'accessing' }
OSSDL2WindowRendererTexture >> extent [
	^ extent
]

{ #category : 'accessing' }
OSSDL2WindowRendererTexture >> extent: anObject [
	extent := anObject
]

{ #category : 'accessing' }
OSSDL2WindowRendererTexture >> handle [
	^ handle
]

{ #category : 'accessing' }
OSSDL2WindowRendererTexture >> handle: anObject [
	handle := anObject
]

{ #category : 'texture manipulation' }
OSSDL2WindowRendererTexture >> uploadAthensSurface: athensSurface [
	handle updateTexturePixels: athensSurface getDataPtr pitch: athensSurface stride
]

{ #category : 'texture manipulation' }
OSSDL2WindowRendererTexture >> uploadForm: aForm [
	| convertedForm |
	convertedForm := aForm asFormOfDepth: 32.
	convertedForm unhibernate.
	handle updateTexturePixels: convertedForm bits pitch: convertedForm width * 4
]
