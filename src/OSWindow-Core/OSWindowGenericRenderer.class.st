"
I am an abstract renderer that provides a generic API for native window rendering.
"
Class {
	#name : #OSWindowGenericRenderer,
	#superclass : #OSWindowRenderer,
	#category : #'OSWindow-Core-Renderer'
}

{ #category : #rendering }
OSWindowGenericRenderer >> clear [
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> color: aColor [
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
OSWindowGenericRenderer >> fillRectangle: aRectangle [
	self subclassResponsibility
]

{ #category : #rendering }
OSWindowGenericRenderer >> present [
	"This should present the content of an internal draw buffer"
	self subclassResponsibility
]
