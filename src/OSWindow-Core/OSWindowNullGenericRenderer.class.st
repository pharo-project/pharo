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

{ #category : #rendering }
OSWindowNullGenericRenderer >> fillRectangle: aRectangle [
	"Do nothing"
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
