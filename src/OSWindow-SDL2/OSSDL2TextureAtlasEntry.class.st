"
I represent a tile entry inside a large texture texture.
"
Class {
	#name : #OSSDL2TextureAtlasEntry,
	#superclass : #Object,
	#instVars : [
		'texture',
		'sourceRectangle',
		'offset',
		'advance',
		'linearAdvance'
	],
	#pools : [
		'SDL2Constants'
	],
	#category : #'OSWindow-SDL2-Rendering'
}

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> advance [
	^ advance
]

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> advance: anObject [
	advance := anObject
]

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> linearAdvance [
	^ linearAdvance
]

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> linearAdvance: anObject [
	linearAdvance := anObject
]

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> offset [
	^ offset
]

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> offset: anObject [
	offset := anObject
]

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> sourceRectangle [
	^ sourceRectangle
]

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> sourceRectangle: anObject [
	sourceRectangle := anObject
]

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> texture [
	^ texture
]

{ #category : #accessing }
OSSDL2TextureAtlasEntry >> texture: anObject [
	texture := anObject
]
