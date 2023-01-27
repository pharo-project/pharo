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
		'deferUpdates',
		'mutex'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #'OSWindow-SDL2-Rendering'
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
	renderer := backendWindow sdl2Window createDefaultRenderer.
]

{ #category : #private }
OSSDL2FormRenderer >> createTexture [
	"Existing texture released explicitly to avoid memory leak on resize (issue#11183)"
	texture ifNotNil: [ texture destroy ].
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
	mutex critical: [
		(texture isNil or: [ renderer isNil ])
			ifTrue: [ ^ self ].
		texture destroy.
		renderer destroy.
		texture := nil.
		renderer := nil.
	]
]

{ #category : #initialization }
OSSDL2FormRenderer >> initialize [
	super initialize.
	deferUpdates := false.
	mutex := Mutex new.
]

{ #category : #private }
OSSDL2FormRenderer >> primitiveUpdateRectangle: rectangle externalForm: externalForm [

	"We need to do this ugly hack because the Mutex that is used in the renderer is not correctly liberated when there is an exception. Issue #10156. Once it is fixed, we can signal the exception as normal people."

	[externalForm copy: rectangle from: form to: rectangle origin rule: Form over]
		on:Exception do: [ :e | ('There was an error during copy, we should not throw an exception here:' , e printString) traceCr ]
]

{ #category : #'updating screen' }
OSSDL2FormRenderer >> updateAll [
	mutex critical: [
		self validate
			ifFalse: [ ^ self ].
	
		texture updateTexturePixels: form bits pitch: form width * 4.
	
		deferUpdates
			ifTrue: [ ^ self ].
		renderer copy: texture.
		renderer present
	]
]

{ #category : #'updating screen' }
OSSDL2FormRenderer >> updateAreas: allDamage immediate: forceToScreen [

	| pixels pitch intersection updateUnion externalForm |
	mutex critical: [ 
		self validate ifFalse: [ ^ self ].

		pixels := ExternalAddress new.
		ExternalAddress allocate: 4 bytesDuring: [ :pitchHolder | 
			texture lockPixels: pixels pitch: pitchHolder.
			pitch := pitchHolder signedLongAt: 1 ].

		updateUnion := nil.
		[ 
		externalForm := OSSDL2ExternalForm
			                extent: extent
			                depth: 32
			                bits: pixels.
		allDamage do: [ :rectangle | 
			intersection := rectangle
				                intersect: (0 @ 0 corner: extent)
				                ifNone: [ nil ].

			intersection ifNotNil: [ 
				self
					primitiveUpdateRectangle: rectangle
					externalForm: externalForm.
				updateUnion
					ifNil: [ updateUnion := intersection ]
					ifNotNil: [ updateUnion := updateUnion merge: intersection ] ] ] ] 
			ensure: [ 
				texture unlock.
				externalForm destroySurface ].

		updateUnion ifNotNil: [ 
			self copyAndPresentTextureRectangle: updateUnion ] ]
]

{ #category : #'updating screen' }
OSSDL2FormRenderer >> updateRectangle: rectangle [

	| intersection pixels pitch externalForm |
	mutex critical: [ 
		intersection := rectangle
			                intersect: (0 @ 0 corner: backendWindow extent)
			                ifNone: [ ^ self ].
		self validate ifFalse: [ ^ self ].

		pixels := ExternalAddress new.
		ExternalAddress allocate: 4 bytesDuring: [ :pitchHolder | 
			texture lockPixels: pixels pitch: pitchHolder.
			pitch := pitchHolder signedLongAt: 1 ].

		[ 
		externalForm := OSSDL2ExternalForm
			                extent: extent
			                depth: 32
			                bits: pixels.
		self
			primitiveUpdateRectangle: intersection
			externalForm: externalForm ] ensure: [ 
			texture unlock.
			externalForm destroySurface ].

		self copyAndPresentTextureRectangle: intersection ]
]

{ #category : #private }
OSSDL2FormRenderer >> useDeferredUpdates [
	mutex critical: [ 
		renderer
			copy: texture;
			present
	]
]

{ #category : #private }
OSSDL2FormRenderer >> validate [
	form ifNil: [ ^ false ].
	(renderer isNil or: [renderer isNull]) ifTrue: [ self createRenderer ].
	(texture isNil or: [texture isNull or: [ form extent ~= extent ]])
		ifTrue: [ self createTexture ].
	^ true
]
