"
I am an abstract texture that is created by an OSWindowGenericRenderer.

The user data member is provided as a quick storage slot for storing additional metadata corresponding to this texture with a constant access guarantee. This slot is typically used for caching the rendering of a sub scene that is rendered through myself.
"
Class {
	#name : 'OSWindowRendererTexture',
	#superclass : 'Object',
	#instVars : [
		'modificationCount',
		'userData'
	],
	#category : 'OSWindow-Core-Renderer',
	#package : 'OSWindow-Core',
	#tag : 'Renderer'
}

{ #category : 'accessing' }
OSWindowRendererTexture >> extent [
	self subclassResponsibility
]

{ #category : 'accessing' }
OSWindowRendererTexture >> modificationCount [
	^ modificationCount
]

{ #category : 'accessing' }
OSWindowRendererTexture >> modificationCount: anObject [
	modificationCount := anObject
]

{ #category : 'texture manipulation' }
OSWindowRendererTexture >> uploadAthensSurface: athensSurface [
]

{ #category : 'texture manipulation' }
OSWindowRendererTexture >> uploadAthensSurface: anAthensSurface withModificationCount: newModificationCount [
	(modificationCount isNil or: [ modificationCount < newModificationCount ]) ifTrue: [
		self uploadAthensSurface: anAthensSurface.
		modificationCount := newModificationCount
	]
]

{ #category : 'texture manipulation' }
OSWindowRendererTexture >> uploadForm: aForm [
]

{ #category : 'texture manipulation' }
OSWindowRendererTexture >> uploadForm: aForm withModificationCount: newModificationCount [
	(modificationCount isNil or: [ modificationCount < newModificationCount ]) ifTrue: [
		self uploadForm: aForm.
		modificationCount := newModificationCount
	]
]

{ #category : 'accessing' }
OSWindowRendererTexture >> userData [
	^ userData
]

{ #category : 'accessing' }
OSWindowRendererTexture >> userData: anObject [
	userData := anObject
]
