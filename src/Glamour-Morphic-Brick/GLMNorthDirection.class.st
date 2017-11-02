Class {
	#name : #GLMNorthDirection,
	#superclass : #GLMDirection,
	#category : 'Glamour-Morphic-Brick-Utils-Geometry'
}

{ #category : #arithmetic }
GLMNorthDirection >> * aNumber [
	<return: #Point>

	^ 0 @ aNumber negated
]

{ #category : #direction }
GLMNorthDirection >> left [
	^ GLMWestDirection uniqueInstance
]

{ #category : #direction }
GLMNorthDirection >> origin: aPoint length: aLength breadth: aBreadth [
	^ aPoint - (0 @ aLength) extent: aBreadth @ aLength
]

{ #category : #direction }
GLMNorthDirection >> rightAngle: aDirection [
	^ aDirection = GLMWestDirection uniqueInstance
]
