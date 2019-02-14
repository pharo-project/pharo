Class {
	#name : #GLMDirection,
	#superclass : #Object,
	#traits : 'TSingleton',
	#classTraits : 'TSingleton classTrait',
	#category : #'Glamour-Morphic-Brick-Utils-Geometry'
}

{ #category : #accessing }
GLMDirection class >> cleanUp [
	super cleanUp.
	
	self reset
]

{ #category : #arithmetic }
GLMDirection >> * aNumber [
	<return: #Point>

	^ self subclassResponsibility
]

{ #category : #direction }
GLMDirection >> left [
	"Return a direction which is a left projection"
	^ self subclassResponsibility
]

{ #category : #direction }
GLMDirection >> origin: aPoint length: aLength breadth: aBreadth [
	^ self subclassResponsibility
]

{ #category : #direction }
GLMDirection >> rightAngle: aDirection [
	^ self subclassResponsibility
]
