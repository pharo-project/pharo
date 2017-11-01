Class {
	#name : #GLMEastDirection,
	#superclass : #GLMDirection,
	#category : 'Glamour-Morphic-Brick-Utils-Geometry'
}

{ #category : #arithmetic }
GLMEastDirection >> * aNumber [
	<return: #Point>
	
	^ aNumber @ 0
]

{ #category : #direction }
GLMEastDirection >> left [
	^ GLMNorthDirection uniqueInstance
]

{ #category : #direction }
GLMEastDirection >> origin: aPoint length: aLength breadth: aBreadth [
	^ aPoint extent: aLength @ aBreadth
]

{ #category : #direction }
GLMEastDirection >> rightAngle: aDirection [
	^ aDirection = GLMNorthDirection uniqueInstance
]
