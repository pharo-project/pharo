"
I am an abstract texture that is created by an OSWindowGenericRenderer.

The user data member is provided as a quick storage slot for storing additional metadata corresponding to this texture with a constant access guarantee. This slot is typically used for caching the rendering of a sub scene that is rendered through myself.
"
Class {
	#name : #OSWindowRendererTexture,
	#superclass : #Object,
	#instVars : [
		'userData'
	],
	#category : #'OSWindow-Core-Renderer'
}

{ #category : #accessing }
OSWindowRendererTexture >> extent [
	self subclassResponsibility
]

{ #category : #accessing }
OSWindowRendererTexture >> userData [
	^ userData
]

{ #category : #accessing }
OSWindowRendererTexture >> userData: anObject [
	userData := anObject
]
