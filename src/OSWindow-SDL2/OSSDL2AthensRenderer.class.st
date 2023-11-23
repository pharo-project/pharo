"
I am an Athens renderer which uses SDL2.
"
Class {
	#name : 'OSSDL2AthensRenderer',
	#superclass : 'OSWindowAthensRenderer',
	#instVars : [
		'texture',
		'renderer',
		'textureExtent',
		'mutex'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : 'OSWindow-SDL2-Rendering',
	#package : 'OSWindow-SDL2',
	#tag : 'Rendering'
}

{ #category : 'instance creation' }
OSSDL2AthensRenderer class >> for: aWindowHandle [
	^ self basicNew
		initializeWindowHandle: aWindowHandle;
		yourself
]

{ #category : 'instance creation' }
OSSDL2AthensRenderer class >> new [
	self error: 'Use #for:'
]

{ #category : 'accessing' }
OSSDL2AthensRenderer >> athensSurface [
	^ mutex critical: [super athensSurface]
]

{ #category : 'deleting' }
OSSDL2AthensRenderer >> destroy [

	mutex critical: [
		texture ifNotNil: [ texture destroy ].
		texture := nil.
		renderer ifNotNil: [ renderer destroy ].
		renderer := nil.
	].

	super destroy
]

{ #category : 'private' }
OSSDL2AthensRenderer >> drawTexturePixelsWith: aBlock [
	| pixels pitch externalForm |
	pixels := ExternalAddress new.
	pitch := ExternalAddress allocate: 4 bytesDuring: [:pitchHolder |
		texture lockPixels: pixels pitch: pitchHolder.
		pitchHolder signedLongAt: 1 ].

	externalForm := OSSDL2ExternalForm extent: textureExtent depth: 32 bits: pixels.
	[
		aBlock value: externalForm
	] ensure: [
		texture unlock.
		externalForm destroySurface
	]
]

{ #category : 'initialization' }
OSSDL2AthensRenderer >> initialize [
	super initialize.
	mutex := Mutex new
]

{ #category : 'initialization' }
OSSDL2AthensRenderer >> initializeWindowHandle: aBackendWindow [
	self initialize.
	self backendWindow: aBackendWindow.
	renderer := backendWindow sdl2Window createDefaultRenderer.
	mutex critical: [
		self resetResources.
	]
]

{ #category : 'updating screen' }
OSSDL2AthensRenderer >> pixelExtent [
	mutex critical: [
		(renderer isNil or: [ renderer isNull ]) ifFalse: [ ^ renderer outputExtent ].
	].
	^ super pixelExtent
]

{ #category : 'drawing' }
OSSDL2AthensRenderer >> prepareForDrawing [
	mutex critical: [
		textureExtent ~= renderer outputExtent ifTrue: [ self resized ].
	]
]

{ #category : 'updating screen' }
OSSDL2AthensRenderer >> present [
	mutex critical: [
		self validate ifFalse: [ ^ self ].
		renderer
			clear;
			copy: texture;
			present
	]
]

{ #category : 'private' }
OSSDL2AthensRenderer >> primitiveUpdateRectangle: rectangle externalForm: externalForm [
	externalForm getCanvas
		drawImage: athensSurface asForm at: rectangle origin sourceRect: rectangle
]

{ #category : 'updating resources' }
OSSDL2AthensRenderer >> resetResources [
	| extent |
	self checkSession.
	extent := renderer outputExtent.
	athensSurface := AthensCairoSurface extent: extent.
	texture ifNotNil: [
		texture destroy.
		texture := nil.
	].
	texture := renderer
		createTextureFormat: SDL_PIXELFORMAT_XRGB8888
		access: SDL_TEXTUREACCESS_STREAMING
		width: extent x
		height: extent y.
	textureExtent := extent
]

{ #category : 'updating screen' }
OSSDL2AthensRenderer >> resized [
	mutex critical: [
		self resetResources.
		super resized
	]
]

{ #category : 'accessing' }
OSSDL2AthensRenderer >> surface [
	^ mutex critical: [super surface]
]

{ #category : 'updating screen' }
OSSDL2AthensRenderer >> updateAll [
	mutex critical: [
		self validate ifFalse: [ ^ self ].

		self drawTexturePixelsWith: [ :textureForm |
			self primitiveUpdateRectangle: (0 @ 0 corner: textureExtent) externalForm: textureForm.
		].
	]
]

{ #category : 'updating screen' }
OSSDL2AthensRenderer >> updateRectangle: aRectangle [
	mutex critical: [
		| intersection |
		self validate ifFalse: [ ^ self ].
		intersection := aRectangle
			intersect: (0 @ 0 corner: textureExtent)
			ifNone: [ ^ self ].

		self drawTexturePixelsWith: [ :textureForm |
			self primitiveUpdateRectangle: intersection externalForm: textureForm
		].
	]
]

{ #category : 'updating screen' }
OSSDL2AthensRenderer >> updateRectangles: allDamage [
	mutex critical: [
		| fullBounds |
		self validate ifFalse: [ ^ self ].

		fullBounds := 0 @ 0 corner: textureExtent.
		self drawTexturePixelsWith: [ :textureForm |
			| intersection |
			allDamage do: [ :rectangle |
				intersection := rectangle intersect: fullBounds ifNone: [ nil ].
				intersection ifNotNil: [
					self primitiveUpdateRectangle: rectangle externalForm: textureForm.
				]
			].
		].
	]
]

{ #category : 'updating screen' }
OSSDL2AthensRenderer >> validate [
	self checkSession.
	athensSurface ifNil: [ ^ false ].
	(texture isNil or: [ texture isNull ]) ifTrue: [ ^ false ].
	(renderer isNil or: [ renderer isNull ]) ifTrue: [ ^ false ].
	renderer outputExtent ~= textureExtent ifTrue: [ ^ false ].
	^ true
]
