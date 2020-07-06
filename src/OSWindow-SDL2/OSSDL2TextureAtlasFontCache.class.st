"
I am a font cache that stores multiple glyphs tiles inside a large texture.
"
Class {
	#name : #OSSDL2TextureAtlasFontCache,
	#superclass : #Object,
	#instVars : [
		'font',
		'renderer',
		'glyphCache',
		'height',
		'destinationX',
		'destinationY',
		'currentRowHeight',
		'form',
		'glyphFormCache',
		'texture',
		'fontBlendMode',
		'activeBlendMode',
		'activeModR',
		'activeModG',
		'activeModB',
		'activeModA',
		'firstPassBlendMode',
		'secondPassBlendMode'
	],
	#classVars : [
		'FontTextureWidth'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #'OSWindow-SDL2-Rendering'
}

{ #category : #'class initialization' }
OSSDL2TextureAtlasFontCache class >> initialize [
	FontTextureWidth := 2048.
]

{ #category : #rendering }
OSSDL2TextureAtlasFontCache >> blendMode: newBlendMode [
	activeBlendMode := newBlendMode.
	texture ifNotNil: [
		texture blendMode: newBlendMode
	].
]

{ #category : #'glyph entries' }
OSSDL2TextureAtlasFontCache >> createTextureGlyphEntryFor: glyphForm [
	| position glyphEntryBounds glyphEntry |
	glyphForm ifNil: [ ^ nil ].
	glyphFormCache at: glyphForm ifPresent: [ :existent | ^ existent ].
	self ensureSpaceFor: glyphForm extent.

	"Copy the glyph into the atlas."
	position := destinationX @ destinationY.
	glyphEntryBounds := (position extent: glyphForm extent) asSDLRect.
	(glyphForm width > 0 and: [ glyphForm height > 0 ]) ifTrue: [ 
		form getCanvas drawImage: glyphForm at: position.
		glyphForm unhibernate.
		texture updateTextureRect: glyphEntryBounds pixels: glyphForm bits pitch: glyphForm width * 4
	].
	
	destinationX := destinationX + glyphForm width + 1.
	
	glyphEntry := OSSDL2TextureAtlasEntry new
		texture: texture;
		sourceRectangle: glyphEntryBounds;
		offset: glyphForm offset;
		advance: glyphForm advance;
		linearAdvance: glyphForm linearAdvance;
		yourself.
	glyphFormCache at: glyphForm put: glyphEntry.
	^ glyphEntry
]

{ #category : #drawing }
OSSDL2TextureAtlasFontCache >> displayStrikeoutFrom: startPoint to: endPoint color: color [
	| r g b a thickness top rect |
	thickness := font strikeoutThickness ceiling.
	thickness <= 0 ifTrue: [ ^ self ].

	r := self mapColorChannel: color red.
	g := self mapColorChannel: color green.
	b := self mapColorChannel: color blue.
	a := self mapColorChannel: color alpha.

	top := font strikeoutTop.
	rect := SDL_Rect new
		x: startPoint x floor;
		y: (startPoint y + top) floor;
		w: (endPoint x - startPoint x) floor;
		h: thickness.
	renderer
		drawColorR: r g: g b: b a: a;
		fillRect: rect
]

{ #category : #drawing }
OSSDL2TextureAtlasFontCache >> displayString: aString color: color from: startIndex to: stopIndex at: aPoint kern: kern [
	| startPosition startPositionX startPositionY r g b a endPoint |
	startPosition := aPoint.

	startPositionX := startPosition x + kern.
	startPositionY := startPosition y + font ascent.
	font hasSubPixelAntiAliasing ifTrue: [
		| alphaFactor |
		alphaFactor := color alpha. 
		r := self mapColorChannel: color red*alphaFactor.
		g := self mapColorChannel: color green*alphaFactor.
		b := self mapColorChannel: color blue*alphaFactor.
		a := self mapColorChannel: alphaFactor.

		"Sub pixel rendering. We need to do this in two passes."
		"Pass 1: remove the color mask and apply the alpha"
		self blendMode: self firstPassBlendMode; modulationR: a g: a b: a a: a.
		self doDisplayString: aString from: startIndex to: stopIndex kern: kern startPositionX: startPositionX startPositionY: startPositionY.

		"Pass 2: apply the actual color"
		self blendMode: self secondPassBlendMode; modulationR: r g: g b: b a: 255.
		endPoint := self doDisplayString: aString from: startIndex to: stopIndex kern: kern startPositionX: startPositionX startPositionY: startPositionY.
	] ifFalse: [ 
		r := self mapColorChannel: color red.
		g := self mapColorChannel: color green.
		b := self mapColorChannel: color blue.
		a := self mapColorChannel: color alpha.

		self modulationR: r g: g b: b a: a.
		endPoint := self doDisplayString: aString from: startIndex to: stopIndex kern: kern startPositionX: startPositionX startPositionY: startPositionY
	].
	^ endPoint
]

{ #category : #drawing }
OSSDL2TextureAtlasFontCache >> displayUnderlineFrom: startPoint to: endPoint color: color [
	| r g b a thickness top rect |
	thickness := font underlineThickness ceiling.
	thickness <= 0 ifTrue: [ ^ self ].

	r := self mapColorChannel: color red.
	g := self mapColorChannel: color green.
	b := self mapColorChannel: color blue.
	a := self mapColorChannel: color alpha.

	top := font underlineTop.
	rect := SDL_Rect new
		x: startPoint x floor;
		y: (startPoint y + top) floor;
		w: (endPoint x - startPoint x) floor;
		h: thickness.
	renderer
		drawColorR: r g: g b: b a: a;
		fillRect: rect
]

{ #category : #private }
OSSDL2TextureAtlasFontCache >> doDisplayString: aString from: startIndex to: stopIndex kern: kern startPositionX: startPositionX startPositionY: startPositionY [
	| widthAndKernedWidth destPositionX destPositionY |
	widthAndKernedWidth := Array new: 2.
	
	destPositionX := startPositionX.
	destPositionY := startPositionY.
	
	startIndex to: stopIndex do: [ :i |
		| char nextChar glyph |
		char := aString at: i.
		
		nextChar := (i + 1 <= stopIndex) 
				ifTrue:[aString at: i + 1]
				ifFalse:[nil].
		glyph := self glyphForCodePoint: char.
		glyph ifNotNil: [
			| glyphRectangle glyphOffset |
			glyphRectangle := glyph sourceRectangle.
			glyphOffset := glyph offset.
			renderer copy: texture srcRect: glyphRectangle dstRect: (SDL_Rect new
				x: (destPositionX + glyphOffset x) floor;
				y: (destPositionY + glyphOffset y) floor;
				w: glyphRectangle w;
				h: glyphRectangle h;
				yourself)
		].
	
		font 
			widthAndKernedWidthOfLeft: char 
			right: nextChar
			into: widthAndKernedWidth.
		destPositionX := destPositionX + (widthAndKernedWidth at: 2)
	].
	^ destPositionX @ destPositionY
]

{ #category : #'texture construction' }
OSSDL2TextureAtlasFontCache >> ensureLatin1 [
	0 to: 255 do: [ :i | self glyphForCodePoint: i ]
]

{ #category : #private }
OSSDL2TextureAtlasFontCache >> ensureSpaceFor: anExtent [
	| nextWidth nextHeight |
	nextWidth := anExtent x.
	nextHeight := anExtent y.
	
	destinationX + nextWidth + 1 > FontTextureWidth ifTrue: [
		destinationX := 1.
		destinationY := destinationY + currentRowHeight + 1.
		currentRowHeight := 0.
	].

	currentRowHeight := currentRowHeight max: nextHeight.
	destinationY + currentRowHeight + 1 > height ifTrue: [
		self increaseHeighTo: (destinationY + currentRowHeight + 1) asLargerPowerOfTwo
	].

]

{ #category : #private }
OSSDL2TextureAtlasFontCache >> firstPassBlendMode [
	^ firstPassBlendMode ifNil: [firstPassBlendMode := SDL2 composeCustomBlendMode_srcColorFactor: SDL_BLENDFACTOR_ZERO dstColorFactor: SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR colorOperation: SDL_BLENDOPERATION_ADD
		srcAlphaFactor: SDL_BLENDFACTOR_ZERO dstAlphaFactor: SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA alphaOperation: SDL_BLENDOPERATION_ADD ]
]

{ #category : #private }
OSSDL2TextureAtlasFontCache >> fontBlendMode [
	^ fontBlendMode ifNil: [fontBlendMode := SDL2 composeCustomBlendMode_srcColorFactor: SDL_BLENDFACTOR_ONE dstColorFactor: SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA colorOperation: SDL_BLENDOPERATION_ADD
		srcAlphaFactor: SDL_BLENDFACTOR_ONE dstAlphaFactor: SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA alphaOperation: SDL_BLENDOPERATION_ADD]
]

{ #category : #'texture construction' }
OSSDL2TextureAtlasFontCache >> glyphForCodePoint: codePoint [
	^ glyphCache at: codePoint ifAbsentPut: [
		self createTextureGlyphEntryFor: (font characterRenderingOptimizedFormAt: codePoint asCharacter)
	].

]

{ #category : #private }
OSSDL2TextureAtlasFontCache >> increaseHeighTo: newHeight [
	| newForm newTexture |
	newForm := Form extent: FontTextureWidth@newHeight depth: 32.
	newTexture := renderer createTextureFormat: SDL_PIXELFORMAT_ARGB8888 access: SDL_TEXTUREACCESS_STATIC width: FontTextureWidth height: newHeight.
	form ifNotNil: [
		newForm getCanvas drawImage: form at: 0@0.
		newForm unhibernate.
		newTexture
			updateTexturePixels: newForm bits pitch: newForm width * 4;
			blendMode: activeBlendMode; colorModR: activeModR g: activeModG b: activeModB; alphaMod: activeModA
	].
	form := newForm.
	texture := newTexture.
	
]

{ #category : #initialization }
OSSDL2TextureAtlasFontCache >> initializeWithFont: theFont renderer: theRenderer [
	font := theFont.
	renderer := theRenderer.
	glyphCache := Dictionary new.
	glyphFormCache := Dictionary new.
	
	activeBlendMode := self fontBlendMode.
	activeModR := activeModG := activeModB := activeModA := 255.
	
	height := 0.
	destinationX := 1.
	destinationY := 1.
	currentRowHeight := 0.
	self ensureLatin1.
]

{ #category : #private }
OSSDL2TextureAtlasFontCache >> mapColorChannel: colorChannel [
	^ colorChannel * 255 asInteger min: 255 max: 0
]

{ #category : #private }
OSSDL2TextureAtlasFontCache >> modulationR: modR g: modG b: modB a: modA [
	activeModR := modR.
	activeModG := modG.
	activeModB := modB.
	activeModA := modA.
	texture ifNotNil: [
		texture colorModR: modR g: modG b: modB; alphaMod: modA
	]
]

{ #category : #private }
OSSDL2TextureAtlasFontCache >> secondPassBlendMode [
	^ secondPassBlendMode ifNil: [secondPassBlendMode := SDL2 composeCustomBlendMode_srcColorFactor: SDL_BLENDFACTOR_ONE dstColorFactor: SDL_BLENDFACTOR_ONE colorOperation: SDL_BLENDOPERATION_ADD
		srcAlphaFactor: SDL_BLENDFACTOR_ONE dstAlphaFactor: SDL_BLENDFACTOR_ONE alphaOperation: SDL_BLENDOPERATION_ADD ]
]
