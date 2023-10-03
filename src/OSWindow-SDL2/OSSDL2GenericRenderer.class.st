"
My instances implement OSWindowGenericRenderer interface by wrapping a SDL2 renderer.
"
Class {
	#name : 'OSSDL2GenericRenderer',
	#superclass : 'OSWindowGenericRenderer',
	#instVars : [
		'renderer',
		'staticTextureCache',
		'activeBlendMode',
		'activeRed',
		'activeGreen',
		'activeBlue',
		'activeAlpha',
		'renderTargetTextureCache',
		'currentRenderTarget',
		'textureForObjectCache',
		'subpixelFontFirstPassBlendMode',
		'subpixelFontSecondPassBlendMode',
		'premultipliedCompositeAlphaBlending'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : 'OSWindow-SDL2-Rendering',
	#package : 'OSWindow-SDL2',
	#tag : 'Rendering'
}

{ #category : 'rendering' }
OSSDL2GenericRenderer >> clear [
	renderer clear
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> clippingBounds: aRectangle [
	renderer setClipRect: aRectangle asSDLRect
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> color: aColor [
	activeRed := self mapColorChannel: aColor red.
	activeGreen := self mapColorChannel: aColor green.
	activeBlue := self mapColorChannel: aColor blue.
	activeAlpha := self mapColorChannel: aColor alpha.
	renderer drawColorR: activeRed g: activeGreen b: activeBlue a: activeAlpha
]

{ #category : 'resources' }
OSSDL2GenericRenderer >> createRenderTargetTextureWithExtent: anExtent [
	| texture |
	texture := renderer createTextureFormat: SDL_PIXELFORMAT_ARGB8888 access: SDL_TEXTUREACCESS_TARGET width: anExtent x height: anExtent y.
	^ OSSDL2WindowRendererTexture new
		extent: anExtent copy;
		handle: texture;
		yourself
]

{ #category : 'initialization' }
OSSDL2GenericRenderer >> createSDL2Renderer [
	renderer := backendWindow sdl2Window createAcceleratedRenderer.

	self useNoBlending;
		color: Color white
]

{ #category : 'private' }
OSSDL2GenericRenderer >> createStaticTextureFromForm32: form [
	| texture |
	self assert: form depth = 32.
	texture := renderer createTextureFormat: SDL_PIXELFORMAT_ARGB8888 access: SDL_TEXTUREACCESS_STATIC width: form width height: form height.
	form unhibernate.
	texture updateTexturePixels: form bits pitch: form width * 4.
	^ OSSDL2WindowRendererTexture new
		extent: form extent copy;
		handle: texture;
		yourself
]

{ #category : 'private' }
OSSDL2GenericRenderer >> createStaticTextureFromForm: form [
	"form depth = 8 ifTrue: [ self halt ].
	form depth =	 16 ifTrue: [ self halt ].
	form depth =	 24 ifTrue: [ self halt ]."
	form depth =	 32 ifTrue: [ ^ self createStaticTextureFromForm32: form ].
	self flag: 'TODO: Support more depths to optimize the memory usage.'.
	^ self createStaticTextureFromForm: (form asFormOfDepth: 32)
]

{ #category : 'resources' }
OSSDL2GenericRenderer >> createTextureForObjectWithExtent:  anExtent [
	| texture |
	texture := renderer createTextureFormat: SDL_PIXELFORMAT_ARGB8888 access: SDL_TEXTUREACCESS_STATIC width: anExtent x height: anExtent y.
	^ OSSDL2WindowRendererTexture new
		extent: anExtent copy;
		handle: texture;
		yourself
]

{ #category : 'deleting' }
OSSDL2GenericRenderer >> destroy [
	(renderer isNotNil and: [renderer isNull not]) ifTrue: [
		renderer destroy
	].
	renderer := nil
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> drawLine: startPoint end: endPoint [
	renderer drawLineX1: startPoint x asInteger y1: startPoint y asInteger x2: endPoint x asInteger y2: endPoint y asInteger
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> drawPoint: aPoint [
	renderer drawPointX: aPoint x asInteger y: aPoint y asInteger
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> drawRectangle: aRectangle [
	renderer drawRect: aRectangle asSDLRect
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> drawTexture: texture in: aRectangle [
	self setupTextureBlendingState: texture handle.
	renderer copy: texture handle srcRect: nil dstRect: aRectangle asSDLRect
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> drawTexture: texture rectangle: sourceRectangle in: destRectangle [
	self setupTextureBlendingState: texture handle.
	renderer copy: texture handle srcRect: sourceRectangle asSDLRect dstRect: destRectangle asSDLRect
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> fillRectangle: aRectangle [
	renderer fillRect: aRectangle asSDLRect
]

{ #category : 'clipping' }
OSSDL2GenericRenderer >> fullClippingExtent [

	^ ExternalAddress allocate: 8 bytesDuring: [ :buffer |
		  | result |
		  renderer getLogicalSizeW: buffer h: buffer + 4.
		  result := (buffer unsignedLongAt: 1) @ (buffer unsignedLongAt: 5).
		  (result x = 0 and: [ result y = 0 ]) ifTrue: [
			  renderer getOutputSizeW: buffer h: buffer + 4.
			  result := (buffer unsignedLongAt: 1) @ (buffer unsignedLongAt: 5) ].
		  result ]
]

{ #category : 'resources' }
OSSDL2GenericRenderer >> getOrCreateRenderTargetTextureFor: anObject withExtent: anExtent [
	| result |
	result := self renderTargetTextureCache at: anObject ifAbsentPut: [ self createRenderTargetTextureWithExtent: anExtent ].
	result extent ~= anExtent ifTrue: [
		result := self createRenderTargetTextureWithExtent: anExtent.
		self renderTargetTextureCache at: anObject put: result
	].
	^ result
]

{ #category : 'resources' }
OSSDL2GenericRenderer >> getOrCreateStaticTextureFromForm: form [
	^ self staticTextureCache at: form ifAbsentPut: [ self createStaticTextureFromForm: form ]
]

{ #category : 'resources' }
OSSDL2GenericRenderer >> getOrCreateTextureFor: anObject withExtent: anExtent [
	| result |
	result := self textureForObjectCache at: anObject ifAbsentPut: [ self createTextureForObjectWithExtent: anExtent ].
	result extent ~= anExtent ifTrue: [
		result := self createTextureForObjectWithExtent: anExtent.
		self textureForObjectCache at: anObject put: result
	].
	^ result
]

{ #category : 'resources' }
OSSDL2GenericRenderer >> getRenderTargetTextureForObjectOrNil: anObject [
	^ self renderTargetTextureCache at: anObject ifAbsent: [ nil ]
]

{ #category : 'resources' }
OSSDL2GenericRenderer >> getTextureForObjectOrNil: anObject [
	^ self textureForObjectCache at: anObject ifAbsent: [ nil ]
]

{ #category : 'private' }
OSSDL2GenericRenderer >> mapColorChannel: colorChannel [
	^ colorChannel * 255 asInteger min: 255 max: 0
]

{ #category : 'clipping' }
OSSDL2GenericRenderer >> pixelExtent [
	^ renderer
		ifNotNil: [ renderer outputExtent ]
		ifNil: [ super pixelExtent ]
]

{ #category : 'blending mode' }
OSSDL2GenericRenderer >> premultipliedCompositeAlphaBlending [
	^ premultipliedCompositeAlphaBlending ifNil: [premultipliedCompositeAlphaBlending :=
		SDL2 composeCustomBlendMode_srcColorFactor: SDL_BLENDFACTOR_ONE dstColorFactor: SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA colorOperation: SDL_BLENDOPERATION_ADD
		srcAlphaFactor: SDL_BLENDFACTOR_ONE dstAlphaFactor: SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA alphaOperation: SDL_BLENDOPERATION_ADD ]
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> present [
	renderer present
]

{ #category : 'private' }
OSSDL2GenericRenderer >> renderTargetTextureCache [
	^ renderTargetTextureCache ifNil: [ renderTargetTextureCache := WeakKeyDictionary new ]
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> setBlendMode: blendMode [
	activeBlendMode := blendMode.
	renderer drawBlendMode: blendMode
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> setupTextureBlendingState: texture [
	texture
		blendMode: activeBlendMode;
		colorModR: activeRed g: activeGreen b: activeBlue;
		alphaMod: activeAlpha
]

{ #category : 'private' }
OSSDL2GenericRenderer >> staticTextureCache [
	^ staticTextureCache ifNil: [ staticTextureCache := WeakKeyDictionary new ]
]

{ #category : 'blending mode' }
OSSDL2GenericRenderer >> subpixelFontFirstPassBlendMode [
	^ subpixelFontFirstPassBlendMode ifNil: [subpixelFontFirstPassBlendMode :=
		SDL2 composeCustomBlendMode_srcColorFactor: SDL_BLENDFACTOR_ZERO dstColorFactor: SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR colorOperation: SDL_BLENDOPERATION_ADD
		srcAlphaFactor: SDL_BLENDFACTOR_ZERO dstAlphaFactor: SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA alphaOperation: SDL_BLENDOPERATION_ADD ]
]

{ #category : 'blending mode' }
OSSDL2GenericRenderer >> subpixelFontSecondPassBlendMode [
	^ subpixelFontSecondPassBlendMode ifNil: [subpixelFontSecondPassBlendMode :=
		SDL2 composeCustomBlendMode_srcColorFactor: SDL_BLENDFACTOR_ONE dstColorFactor: SDL_BLENDFACTOR_ONE colorOperation: SDL_BLENDOPERATION_ADD
		srcAlphaFactor: SDL_BLENDFACTOR_ONE dstAlphaFactor: SDL_BLENDFACTOR_ONE alphaOperation: SDL_BLENDOPERATION_ADD ]
]

{ #category : 'testing' }
OSSDL2GenericRenderer >> supportsRenderTargets [
	^ renderer renderTargetSupported
]

{ #category : 'private' }
OSSDL2GenericRenderer >> textureForObjectCache [
	^ textureForObjectCache ifNil: [ textureForObjectCache := WeakKeyDictionary new ]
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> useAdditiveColorBlending [
	self setBlendMode: SDL_BLENDMODE_ADD
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> useCompositeAlphaBlending [
	self setBlendMode: SDL_BLENDMODE_BLEND
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> useModulatingColorBlending [
	self setBlendMode: SDL_BLENDMODE_MOD
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> useNoBlending [
	self setBlendMode: SDL_BLENDMODE_NONE
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> usePremultipliedCompositeAlphaBlending [
	self setBlendMode: self premultipliedCompositeAlphaBlending
]

{ #category : 'blending mode' }
OSSDL2GenericRenderer >> useSubpixelFontFirstPassBlending [
	self setBlendMode: self subpixelFontFirstPassBlendMode
]

{ #category : 'blending mode' }
OSSDL2GenericRenderer >> useSubpixelFontSecondPassBlending [
	self setBlendMode: self subpixelFontSecondPassBlendMode
]

{ #category : 'rendering' }
OSSDL2GenericRenderer >> withRenderTarget: aRenderTargetTexture do: aBlock [
	| oldRenderTarget |
	oldRenderTarget := currentRenderTarget.
	currentRenderTarget := aRenderTargetTexture ifNotNil: [ aRenderTargetTexture handle ].
	renderer setRenderTarget: currentRenderTarget.
	aBlock ensure: [
		currentRenderTarget := oldRenderTarget.
		renderer setRenderTarget: (currentRenderTarget ifNil: [SDL_Texture null]).
	]
]
