Class {
	#name : #GLMSouthDirection,
	#superclass : #GLMDirection,
	#category : 'Glamour-Morphic-Brick-Utils-Geometry'
}

{ #category : #arithmetic }
GLMSouthDirection >> * aNumber [
	<return: #Point>

	^ 0 @ aNumber
]

{ #category : #direction }
GLMSouthDirection >> left [
	^ GLMEastDirection uniqueInstance
]

{ #category : #direction }
GLMSouthDirection >> origin: aPoint length: aLength breadth: aBreadth [
	"Creates a rectangle to the right of the vector pointing in direction I define"
	^ aPoint - (aBreadth @ 0) extent: aBreadth @ aLength
]

{ #category : #direction }
GLMSouthDirection >> rightAngle: aDirection [
	^ aDirection = GLMEastDirection uniqueInstance
]
