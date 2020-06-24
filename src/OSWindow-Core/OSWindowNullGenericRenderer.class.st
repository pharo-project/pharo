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

{ #category : #'as yet unclassified' }
OSWindowNullGenericRenderer >> clippingBounds: aRectangle [
	"Nothing required here"
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> color: aColor [
	"Do nothing"
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

{ #category : #'as yet unclassified' }
OSWindowNullGenericRenderer >> drawString: aString from: firstIndex to: lastIndex in: bounds font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc kern: kern [
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> drawTexture: texture in: rectangle [
	
]

{ #category : #rendering }
OSWindowNullGenericRenderer >> fillRectangle: aRectangle [
	"Do nothing"
]

{ #category : #'as yet unclassified' }
OSWindowNullGenericRenderer >> getOrCreateStaticTextureFromForm: from [
	"This gets or creates a cached static texture corresponding to a form. This is typically used for drawing GUI icons."
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
