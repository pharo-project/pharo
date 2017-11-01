Class {
	#name : #GLMDirection,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'Glamour-Morphic-Brick-Utils-Geometry'
}

{ #category : #accessing }
GLMDirection class >> cleanUp [
	super cleanUp.
	
	uniqueInstance := nil
]

{ #category : #accessing }
GLMDirection class >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
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
