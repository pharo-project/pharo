Class {
	#name : #GLMWestDirection,
	#superclass : #GLMDirection,
	#category : #'Glamour-Morphic-Brick-Utils-Geometry'
}

{ #category : #testing }
GLMWestDirection class >> isDeprecated [
	^ true
]

{ #category : #arithmetic }
GLMWestDirection >> * aNumber [
	<return: #Point>
	
	^ aNumber negated @ 0
]

{ #category : #direction }
GLMWestDirection >> left [
	^ GLMSouthDirection uniqueInstance
]

{ #category : #direction }
GLMWestDirection >> origin: aPoint length: aLength breadth: aBreadth [
	^ aPoint - (aLength @ aBreadth) extent: aLength @ aBreadth
]

{ #category : #direction }
GLMWestDirection >> rightAngle: aDirection [
	^ aDirection = GLMSouthDirection uniqueInstance
]
