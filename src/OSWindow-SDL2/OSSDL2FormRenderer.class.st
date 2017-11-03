"
My instances implement OSWindowFormRenderer interface using SDL2 library
"
Class {
	#name : #OSSDL2FormRenderer,
	#superclass : #OSWindowFormRenderer,
	#instVars : [
		'texture',
		'renderer',
		'extent',
		'deferUpdates'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #OSWindow-SDL2
}

{ #category : #'updating screen' }
OSSDL2FormRenderer >> copyAndPresentTextureRectangle: rectangle [
	| sdlRectangle |

	deferUpdates
		ifTrue: [ ^ self ].

	sdlRectangle := rectangle asSDLRect.				
	renderer copy: texture srcRect: sdlRectangle dstRect: sdlRectangle.
	renderer present
	
]

{ #category : #private }
OSSDL2FormRenderer >> createRenderer [
	renderer := window handle createDefaultRenderer.

]

{ #category : #private }
OSSDL2FormRenderer >> createTexture [
	texture := renderer createTextureFormat: SDL_PIXELFORMAT_XRGB8888
						access: SDL_TEXTUREACCESS_STREAMING width: form width height: form height.
	extent := form extent
]

{ #category : #'updating screen' }
OSSDL2FormRenderer >> deferUpdatesWhile: aBlock [
	deferUpdates := true.
	aBlock ensure: [ deferUpdates := false. ].
	self useDeferredUpdates.
	
]

{ #category : #deleting }
OSSDL2FormRenderer >> destroy [
	(texture isNil or: [ renderer isNil ])
		ifTrue: [ ^ self ].
	texture destroy.
	renderer destroy
]

{ #category : #initialization }
OSSDL2FormRenderer >> initialize [
	super initialize.
	deferUpdates := false.
]

{ #category : #private }
OSSDL2FormRenderer >> primitiveUpdateRectangle: rectangle externalForm: externalForm [
	externalForm copy: rectangle from: form to: rectangle origin rule: Form over

]

{ #category : #'updating screen' }
OSSDL2FormRenderer >> updateAll [
	self validate
		ifFalse: [ ^ self ].
	
	texture updateTexturePixels: form bits pitch: form width * 4.
	
	deferUpdates
		ifTrue: [ ^ self ].
	renderer copy: texture.
	renderer present
]

{ #category : #'updating screen' }
OSSDL2FormRenderer >> updateAreas: allDamage immediate: forceToScreen [
	| pixels pitchHolder pitch intersection updateUnion externalForm |
	self validate
		ifFalse: [ ^ self ].
		
	pixels := ExternalAddress new.
	pitchHolder := ExternalAddress allocate: 4.
	[
		texture lockPixels: pixels pitch: pitchHolder.
		pitch := pitchHolder signedLongAt: 1.
	] ensure: [ pitchHolder free ].
	
	updateUnion := nil.
	[
		externalForm := OSSDL2ExternalForm extent: extent depth: 32 bits: pixels.
		allDamage do: [ :rectangle |
			intersection := rectangle
				intersect: (0 @ 0 corner: window extent)
				ifNone: [ nil ].
		
			intersection ifNotNil: [
				self primitiveUpdateRectangle: rectangle externalForm: externalForm.
				updateUnion ifNil: [
					updateUnion := intersection 
				] ifNotNil: [
					updateUnion := updateUnion merge: intersection
				]
			]
		].
	] ensure: [
		texture unlock.
		externalForm destroySurface
	].

	updateUnion ifNotNil: [
		self copyAndPresentTextureRectangle: updateUnion
	].

	
]

{ #category : #'updating screen' }
OSSDL2FormRenderer >> updateRectangle: rectangle [
	| intersection pixels pitch pitchHolder externalForm |
	intersection := rectangle
		intersect: (0 @ 0 corner: window extent)
		ifNone: [ ^ self ].
	self validate
		ifFalse: [ ^ self ].
		
	pixels := ExternalAddress new.
	pitchHolder := ExternalAddress allocate: 4.
	[
		texture lockPixels: pixels pitch: pitchHolder.
		pitch := pitchHolder signedLongAt: 1.
	] ensure: [ pitchHolder free ].
	
	[
		externalForm := OSSDL2ExternalForm extent: extent depth: 32 bits: pixels.
		self primitiveUpdateRectangle: intersection externalForm: externalForm
	] ensure: [
		texture unlock.
		externalForm destroySurface
	].

	self copyAndPresentTextureRectangle: intersection.

]

{ #category : #private }
OSSDL2FormRenderer >> useDeferredUpdates [
	renderer
		copy: texture;
		present
]

{ #category : #private }
OSSDL2FormRenderer >> validate [
	form ifNil: [ ^ false ].
	(renderer isNil or: [renderer isNull]) ifTrue: [ self createRenderer ].
	(texture isNil or: [texture isNull or: [ form extent ~= extent ]])
		ifTrue: [ self createTexture ].
	^ true
]
