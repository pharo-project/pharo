Class {
	#name : #GLMArea,
	#superclass : #Object,
	#instVars : [
		'origin',
		'vector',
		'vertices'
	],
	#category : #Glamour-Morphic-Brick-Utils-Geometry
}

{ #category : #initialization }
GLMArea class >> origin: aPoint vectors: theVectors [
	^ self new
		origin: aPoint;
		vectors: theVectors;
		yourself
]

{ #category : #initialization }
GLMArea class >> vectors: theVectors [
	^ self new
		vectors: theVectors;
		yourself
]

{ #category : #private }
GLMArea >> computeVertices [
	vector ifNil: [ ^ self ].
	
	vertices := Array streamContents: [ :aStream |
		| currentVector vertex |
		currentVector := vector.
		vertex := origin.
		aStream nextPut: vertex.
		
		[ currentVector next ~= vector ] whileTrue: [ 
			vertex := currentVector + vertex.
			aStream nextPut: vertex.
			currentVector := currentVector next ] ]
]

{ #category : #initialization }
GLMArea >> initialize [
	super initialize.
	
	origin := 0@0
]

{ #category : #accessing }
GLMArea >> origin [
	^ origin
]

{ #category : #accessing }
GLMArea >> origin: aPoint [

	origin := aPoint.
	self computeVertices
]

{ #category : #accessing }
GLMArea >> vector [
	^ vector
]

{ #category : #initialization }
GLMArea >> vectors: theVectors [
	| currentVector |

	self
		assert: [ theVectors size >= 4 ]
		description: [ 'There must be at least four vectors (rectangular area)!' ].

	vector := theVectors first.
	currentVector := vector.

	2 to: theVectors size do: [ :anIndex |
		| aVector |
		aVector := theVectors at: anIndex.
		currentVector next: aVector.
		aVector previous: currentVector.
		currentVector := aVector ].

	currentVector next: vector.
	vector previous: currentVector.

	self computeVertices
]

{ #category : #accessing }
GLMArea >> vertices [
	^ vertices
]
