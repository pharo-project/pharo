"
My instances implement OSWindowGenericRenderer interface by wrapping a SDL2 renderer.
"
Class {
	#name : #OSSDL2GenericRenderer,
	#superclass : #OSWindowGenericRenderer,
	#instVars : [
		'renderer',
		'staticTextureCache',
		'activeBlendMode',
		'activeRed',
		'activeGreen',
		'activeBlue',
		'activeAlpha',
		'cachedFontCache',
		'renderTargetTextureCache',
		'currentRenderTarget'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #'OSWindow-SDL2-Rendering'
}

{ #category : #accessing }
OSSDL2GenericRenderer >> cachedFontCache [
	^ cachedFontCache ifNil: [cachedFontCache := WeakKeyDictionary new]
]

{ #category : #rendering }
OSSDL2GenericRenderer >> clear [
	renderer clear
]

{ #category : #rendering }
OSSDL2GenericRenderer >> clippingBounds: aRectangle [
	renderer setClipRect: aRectangle asSDLRect
]

{ #category : #rendering }
OSSDL2GenericRenderer >> color: aColor [
	activeRed := self mapColorChannel: aColor red.
	activeGreen := self mapColorChannel: aColor green.
	activeBlue := self mapColorChannel: aColor blue.
	activeAlpha := self mapColorChannel: aColor alpha.
	renderer drawColorR: activeRed g: activeGreen b: activeBlue a: activeAlpha
]

{ #category : #resources }
OSSDL2GenericRenderer >> createRenderTargetTextureWithExtent: anExtent [
	| texture |
	texture := renderer createTextureFormat: SDL_PIXELFORMAT_ARGB8888 access: SDL_TEXTUREACCESS_TARGET width: anExtent x height: anExtent y.
	^ OSSDL2WindowRendererTexture new
		extent: anExtent copy;
		handle: texture;
		yourself
]

{ #category : #initialization }
OSSDL2GenericRenderer >> createSDL2Renderer [
	renderer := backendWindow sdl2Window createAcceleratedRenderer.
	
	self useNoBlending;
		color: Color white.
]

{ #category : #private }
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

{ #category : #private }
OSSDL2GenericRenderer >> createStaticTextureFromForm: form [
	"form depth = 8 ifTrue: [ self halt ].
	form depth =	 16 ifTrue: [ self halt ].
	form depth =	 24 ifTrue: [ self halt ]."
	form depth =	 32 ifTrue: [ ^ self createStaticTextureFromForm32: form ].
	self flag: 'TODO: Support more depths to optimize the memory usage.'.
	^ self createStaticTextureFromForm: (form asFormOfDepth: 32) 
]

{ #category : #private }
OSSDL2GenericRenderer >> createTextureAtlasFontCacheFor: font [
	^ OSSDL2TextureAtlasFontCache new
		initializeWithFont: font renderer: renderer;
		yourself
]

{ #category : #deleting }
OSSDL2GenericRenderer >> destroy [
	renderer destroy
]

{ #category : #rendering }
OSSDL2GenericRenderer >> drawLine: startPoint end: endPoint [
	renderer drawLineX1: startPoint x asInteger y1: startPoint y asInteger x2: endPoint x asInteger y2: endPoint y asInteger
]

{ #category : #rendering }
OSSDL2GenericRenderer >> drawPoint: aPoint [
	renderer drawPointX: aPoint x asInteger y: aPoint y asInteger
]

{ #category : #rendering }
OSSDL2GenericRenderer >> drawRectangle: aRectangle [
	renderer drawRect: aRectangle asSDLRect
]

{ #category : #rendering }
OSSDL2GenericRenderer >> drawString: aString from: startIndex to: stopIndex in: bounds font: font color: color underline: underline underlineColor: underlineColor strikethrough: strikethrough strikethroughColor: strikethroughColor kern: kern [
	| cachedFont endPoint startPointOffset |
	font ifNil: [^ self].
	
	cachedFont := self getOrCreateCachedFontFor: font.
	cachedFont ifNil: [ ^ self ].
	
	endPoint := cachedFont displayString: aString color: color from: startIndex to: stopIndex at: bounds topLeft kern: kern.
	
	startPointOffset := kern @ font ascent.
	underline ifTrue: [
		cachedFont displayUnderlineFrom: bounds topLeft + startPointOffset to: endPoint color: color
	].

	strikethrough ifTrue: [
		cachedFont displayStrikeoutFrom: bounds topLeft + startPointOffset to: endPoint color: color
	].

]

{ #category : #rendering }
OSSDL2GenericRenderer >> drawTexture: texture in: aRectangle [
	self setupTextureBlendingState: texture handle.
	renderer copy: texture handle srcRect: nil dstRect: aRectangle asSDLRect
]

{ #category : #rendering }
OSSDL2GenericRenderer >> fillRectangle: aRectangle [
	renderer fillRect: aRectangle asSDLRect
]

{ #category : #clipping }
OSSDL2GenericRenderer >> fullClippingExtent [
	| buffer |
	buffer := ExternalAddress allocate: 8.
	^ [
		| result |
		renderer getLogicalSizeW: buffer h: buffer + 4.
		result := (buffer unsignedLongAt: 1) @ (buffer unsignedLongAt: 5).
		(result x = 0 and: [ result y = 0 ]) ifTrue: [ 
			renderer getOutputSizeW: buffer h: buffer + 4.
			result := (buffer unsignedLongAt: 1) @ (buffer unsignedLongAt: 5).
		].
		result
	] ensure: [ buffer free ].
]

{ #category : #private }
OSSDL2GenericRenderer >> getOrCreateCachedFontFor: font [
	^ self cachedFontCache at: font ifAbsentPut: [font createCachedFontForSDL2GenericRenderer: self]
]

{ #category : #resources }
OSSDL2GenericRenderer >> getOrCreateRenderTargetTextureFor: anObject withExtent: anExtent [
	| result |
	result := self renderTargetTextureCache at: anObject ifAbsentPut: [ self createRenderTargetTextureWithExtent: anExtent ].
	result extent ~= anExtent ifTrue: [
		result := self createRenderTargetTextureWithExtent: anExtent.
		self renderTargetTextureCache at: anObject put: result
	].
	^ result
]

{ #category : #resources }
OSSDL2GenericRenderer >> getOrCreateStaticTextureFromForm: form [
	^ self staticTextureCache at: form ifAbsentPut: [ self createStaticTextureFromForm: form ]
]

{ #category : #private }
OSSDL2GenericRenderer >> mapColorChannel: colorChannel [
	^ colorChannel * 255 asInteger min: 255 max: 0
]

{ #category : #rendering }
OSSDL2GenericRenderer >> present [
	renderer present
]

{ #category : #private }
OSSDL2GenericRenderer >> renderTargetTextureCache [
	^ renderTargetTextureCache ifNil: [ renderTargetTextureCache := WeakKeyDictionary new ]
]

{ #category : #rendering }
OSSDL2GenericRenderer >> setBlendMode: blendMode [
	activeBlendMode := blendMode.
	renderer drawBlendMode: blendMode
]

{ #category : #rendering }
OSSDL2GenericRenderer >> setupTextureBlendingState: texture [
	texture
		blendMode: activeBlendMode;
		colorModR: activeRed g: activeGreen b: activeBlue;
		alphaMod: activeAlpha
]

{ #category : #private }
OSSDL2GenericRenderer >> staticTextureCache [
	^ staticTextureCache ifNil: [ staticTextureCache := WeakKeyDictionary new ]
]

{ #category : #rendering }
OSSDL2GenericRenderer >> useAdditiveColorBlending [
	self setBlendMode: SDL_BLENDMODE_ADD.
]

{ #category : #rendering }
OSSDL2GenericRenderer >> useCompositeAlphaBlending [
	self setBlendMode: SDL_BLENDMODE_BLEND
]

{ #category : #rendering }
OSSDL2GenericRenderer >> useModulatingColorBlending [
	self setBlendMode: SDL_BLENDMODE_MOD
]

{ #category : #rendering }
OSSDL2GenericRenderer >> useNoBlending [
	self setBlendMode: SDL_BLENDMODE_NONE
]

{ #category : #rendering }
OSSDL2GenericRenderer >> withRenderTarget: aRenderTargetTexture do: aBlock [
	| oldRenderTarget |
	oldRenderTarget := currentRenderTarget.
	currentRenderTarget := aRenderTargetTexture ifNotNil: [ aRenderTargetTexture handle ].
	renderer setRenderTarget: currentRenderTarget.
	aBlock ensure: [
		currentRenderTarget := oldRenderTarget.
		renderer setRenderTarget: (currentRenderTarget ifNil: [SDL_Texture null]).
	].
]
