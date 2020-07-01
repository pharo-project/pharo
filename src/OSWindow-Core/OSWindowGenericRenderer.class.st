"
I am an abstract renderer that provides a generic API for native window rendering.
"
Class {
	#name : #OSWindowGenericRenderer,
	#superclass : #OSWindowRenderer,
	#instVars : [
		'scene'
	],
	#category : #'OSWindow-Core-Renderer'
}

{ #category : #rendering }
OSWindowGenericRenderer >> clear [
	self subclassResponsibility
]

{ #category : #clipping }
OSWindowGenericRenderer >> clippingBounds: aRectangle [
	"This methods sets a new clipping rectangle for the whole renderer output."
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> color: aColor [
	self subclassResponsibility
]

{ #category : #resources }
OSWindowGenericRenderer >> createRenderTargetTextureWithExtent: anExtent [
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> drawLine: startPoint end: endPoint [
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> drawPoint: aPoint [
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> drawRectangle: aRectangle [
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> drawString: aString from: firstIndex to: lastIndex in: bounds font: font color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc kern: kern [
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> drawTexture: texture in: rectangle [
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> fillRectangle: aRectangle [
	self subclassResponsibility
]

{ #category : #clipping }
OSWindowGenericRenderer >> fullClippingBounds [
	"This method should return a clipping rectangle that encompasses the full renderer output surface."
	^ 0@0 extent: self fullClippingExtent
]

{ #category : #clipping }
OSWindowGenericRenderer >> fullClippingExtent [
	"This method should return the extent associated to a clipping rectangle that encompasses the full renderer output surface."
	^ backendWindow extent
]

{ #category : #resources }
OSWindowGenericRenderer >> getOrCreateRenderTargetTextureFor: anObject withExtent: anExtent [
	"This gets or creates a cached render target texture corresponding to an an abstract object used as a key."
	self subclassResponsibility
]

{ #category : #resources }
OSWindowGenericRenderer >> getOrCreateStaticTextureFromForm: from [
	"This gets or creates a cached static texture corresponding to a form. This is typically used for drawing GUI icons."
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> present [
	"This should present the content of an internal draw buffer"
	self subclassResponsibility
]

{ #category : #accessing }
OSWindowGenericRenderer >> scene [
	^ scene
]

{ #category : #accessing }
OSWindowGenericRenderer >> scene: anObject [
	scene := anObject
]

{ #category : #'updating screen' }
OSWindowGenericRenderer >> updateAll [
	scene ifNotNil: [ scene renderAllWith: self ]
]

{ #category : #'updating screen' }
OSWindowGenericRenderer >> updateRectangle: rectangle [
	scene ifNotNil: [ scene renderRectangle: rectangle with: self ]
]

{ #category : #'updating screen' }
OSWindowGenericRenderer >> updateRectangles: aCollectionOfRectangles [
	scene ifNotNil: [ scene renderRectangles: aCollectionOfRectangles with: self ]
]

{ #category : #'blending mode' }
OSWindowGenericRenderer >> useAdditiveColorBlending [
	self subclassResponsibility
]

{ #category : #'blending mode' }
OSWindowGenericRenderer >> useCompositeAlphaBlending [
	self subclassResponsibility
]

{ #category : #'blending mode' }
OSWindowGenericRenderer >> useModulatingColorBlending [
	self subclassResponsibility
]

{ #category : #'blending mode' }
OSWindowGenericRenderer >> useNoBlending [
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> withRenderTarget: aRenderTargetTexture do: aBlock [
	"Activates a specific render target during the block."
	self subclassResponsibility
]
