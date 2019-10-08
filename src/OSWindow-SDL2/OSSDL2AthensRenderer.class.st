"
I am an Athens renderer which uses SDL2.
"
Class {
	#name : #OSSDL2AthensRenderer,
	#superclass : #OSWindowAthensRenderer,
	#instVars : [
		'texture',
		'renderer',
		'pixels',
		'pitch',
		'sdlSurface',
		'textureExtent',
		'surfaceForm'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #'OSWindow-SDL2-Rendering'
}

{ #category : #'instance creation' }
OSSDL2AthensRenderer class >> for: aWindowHandle [
	^ self basicNew 
		initializeWindowHandle: aWindowHandle;
		yourself
]

{ #category : #'instance creation' }
OSSDL2AthensRenderer class >> new [ 
	self error: 'Use #for:'
]

{ #category : #private }
OSSDL2AthensRenderer >> createSDLSurfaceWithExtent: extent [
	surfaceForm ifNotNil: [ surfaceForm destroySurface ].
	"createRGBSurfaceForCairoWidth creates the surface with autoRelease."
	sdlSurface := SDL2 createRGBSurfaceForCairoWidth: extent x height: extent y.
	surfaceForm := OSSDL2ExternalForm extent: extent depth: 32 bits: sdlSurface pixels.
]

{ #category : #deleting }
OSSDL2AthensRenderer >> destroy [
	
	super destroy.
	sdlSurface := nil.
	texture := nil.
	renderer := nil.
]

{ #category : #initialization }
OSSDL2AthensRenderer >> initializeWindowHandle: aWindowHandle [ 
	self initialize.
	self window: aWindowHandle.
	renderer := window handle createDefaultRenderer.
	self resetResources.
	
		
]

{ #category : #drawing }
OSSDL2AthensRenderer >> prepareForDrawing [
	textureExtent ~= window extent ifTrue: [ self resized ].
]

{ #category : #'updating screen' }
OSSDL2AthensRenderer >> present [
	self validate ifFalse: [ ^ self ].
	renderer
		clear;
		copy: texture;
		present
]

{ #category : #private }
OSSDL2AthensRenderer >> primitiveUpdateRectangle: rectangle externalForm: externalForm [
	externalForm copy: rectangle from: surfaceForm to: rectangle origin rule: Form over
]

{ #category : #'updating resources' }
OSSDL2AthensRenderer >> resetResources [
	| extent |
	self checkSession.
	extent := self window extent.
	self createSDLSurfaceWithExtent: extent.
	athensSurface := AthensCairoSDLSurface fromSDLSurface: sdlSurface.
	texture := renderer 
		createTextureFormat: SDL_PIXELFORMAT_XRGB8888 
		access: SDL_TEXTUREACCESS_STREAMING 
		width: extent x 
		height: extent y.
	textureExtent := extent.
]

{ #category : #'updating screen' }
OSSDL2AthensRenderer >> resized [
	self resetResources.
	super resized
]

{ #category : #'updating screen' }
OSSDL2AthensRenderer >> updateAll [
	self validate ifFalse: [ ^ self ].
	texture updateTexturePixels: sdlSurface pixels pitch: sdlSurface pitch.
]

{ #category : #'updating screen' }
OSSDL2AthensRenderer >> updateRectangle: aRectangle [
	| intersection pitchHolder externalForm |
	self validate ifFalse: [ ^ self ].
	intersection := aRectangle
		intersect: (0 @ 0 corner: textureExtent)
		ifNone: [ ^ self ].
	
	pixels := ExternalAddress new.
	pitchHolder := ExternalAddress allocate: 4.
	[
		texture lockPixels: pixels pitch: pitchHolder.
		pitch := pitchHolder signedLongAt: 1.
	] ensure: [ pitchHolder free ].

	externalForm := OSSDL2ExternalForm extent: textureExtent depth: 32 bits: pixels.
	[
		self primitiveUpdateRectangle: intersection externalForm: externalForm
	] ensure: [
		texture unlock.
		externalForm destroySurface
	].

]

{ #category : #'updating screen' }
OSSDL2AthensRenderer >> updateRectangles: allDamage [
	| pitchHolder externalForm intersection |
	self validate
		ifFalse: [ ^ self ].
		
	pixels := ExternalAddress new.
	pitchHolder := ExternalAddress allocate: 4.
	[
		texture lockPixels: pixels pitch: pitchHolder.
		pitch := pitchHolder signedLongAt: 1.
	] ensure: [ pitchHolder free ].
	
	[
		externalForm := OSSDL2ExternalForm extent: textureExtent depth: 32 bits: pixels.
		allDamage do: [ :rectangle |
			intersection := rectangle
				intersect: (0 @ 0 corner: window extent)
				ifNone: [ nil ].
		
			intersection ifNotNil: [
				self primitiveUpdateRectangle: rectangle externalForm: externalForm.
			]
		].
	] ensure: [
		texture unlock.
		externalForm destroySurface
	].

]

{ #category : #'updating screen' }
OSSDL2AthensRenderer >> validate [
	self checkSession.
	(texture isNil or: [ texture isNull ]) ifTrue: [ ^ false ].	
	window extent ~= textureExtent ifTrue: [ ^ false ].
	^ true
]
