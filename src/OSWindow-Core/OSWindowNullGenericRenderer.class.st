"
I am an implementation of the generic API for native window rendering that does nothing.
"
Class {
	#name : #OSWindowNullGenericRenderer,
	#superclass : #OSWindowGenericRenderer,
	#category : #'OSWindow-Core-Renderer'
}

{ #category : #rendering }
OSWindowNullGenericRenderer >> clear [
	"Do nothing"
]

{ #category : #clipping }
OSWindowNullGenericRenderer >> clippingBounds: aRectangle [
	"Nothing required here"
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> color: aColor [
	"Do nothing"
]

{ #category : #resources }
OSWindowNullGenericRenderer >> createRenderTargetTextureWithExtent: anExtent [
	^ nil
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> drawLine: startPoint end: endPoint [
	"Do nothing"
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> drawPoint: aPoint [
	"Do nothing"
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> drawRectangle: aRectangle [
	"Do nothing"
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> drawString: aString from: firstIndex to: lastIndex in: bounds font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc kern: kern [
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> drawTexture: texture in: rectangle [
	
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> fillRectangle: aRectangle [
	"Do nothing"
]

{ #category : #resources }
OSWindowNullGenericRenderer >> getOrCreateRenderTargetTextureFor: anObject withExtent: anExtent [
	^ nil
]

{ #category : #resources }
OSWindowNullGenericRenderer >> getOrCreateStaticTextureFromForm: from [
	^ nil
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> present [
	"Do nothing"
]

{ #category : #'updating screen' }
OSWindowNullGenericRenderer >> updateAll [
	"Do nothing"
]

{ #category : #'updating screen' }
OSWindowNullGenericRenderer >> updateRectangle: rectangle [
	"Do nothing"
]

{ #category : #'updating screen' }
OSWindowNullGenericRenderer >> updateRectangles: aCollectionOfRectangles [
	"Do nothing"
]

{ #category : #'blending mode' }
OSWindowNullGenericRenderer >> useAdditiveColorBlending [
]

{ #category : #'blending mode' }
OSWindowNullGenericRenderer >> useCompositeAlphaBlending [
]

{ #category : #'blending mode' }
OSWindowNullGenericRenderer >> useModulatingColorBlending [
]

{ #category : #'blending mode' }
OSWindowNullGenericRenderer >> useNoBlending [
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> withRenderTarget: aRenderTargetTexture do: aBlock [
	^ aBlock value
]
