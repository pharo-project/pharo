Class {
	#name : #GLMVector,
	#superclass : #Object,
	#instVars : [
		'direction',
		'length',
		'next',
		'previous'
	],
	#classVars : [
		'East',
		'North',
		'South',
		'West'
	],
	#category : 'Glamour-Morphic-Brick-Utils-Geometry'
}

{ #category : #'instance creation' }
GLMVector class >> direction: aDirection [
	^ self new
		direction: aDirection;
		yourself
]

{ #category : #'instance creation' }
GLMVector class >> east: aLength [
	^ self new
		east;
		length: aLength;
		yourself
]

{ #category : #'instance creation' }
GLMVector class >> horizontal: aNumber [
	^ aNumber >= 0
		ifTrue: [ self east: aNumber ]
		ifFalse: [ self west: aNumber abs ]
]

{ #category : #initialization }
GLMVector class >> initialize [
	super initialize.
	
	North := GLMNorthDirection uniqueInstance.
	South := GLMSouthDirection uniqueInstance.
	West := GLMWestDirection uniqueInstance.
	East := GLMEastDirection uniqueInstance 
]

{ #category : #'instance creation' }
GLMVector class >> north: aLength [
	^ self new
		north;
		length: aLength;
		yourself
]

{ #category : #'instance creation' }
GLMVector class >> south: aLength [
	^ self new
		south;
		length: aLength;
		yourself
]

{ #category : #'instance creation' }
GLMVector class >> vertical: aNumber [
	^ aNumber >= 0
		ifTrue: [ self south: aNumber ]
		ifFalse: [ self north: aNumber abs ]
]

{ #category : #'instance creation' }
GLMVector class >> west: aLength [
	^ self new
		west;
		length: aLength;
		yourself
]

{ #category : #geometry }
GLMVector >> * aNumber [
	^ self direction * aNumber
]

{ #category : #geometry }
GLMVector >> + aPoint [
	^ aPoint + self offset
]

{ #category : #accessing }
GLMVector >> direction [
	^ direction
]

{ #category : #initialization }
GLMVector >> direction: aDirection [
	direction := aDirection
]

{ #category : #initialization }
GLMVector >> east [
	direction := East
]

{ #category : #initialization }
GLMVector >> initialize [
	super initialize.
	
	next := self.
	previous := self
]

{ #category : #geometry }
GLMVector >> left [
	"Return a vector representing left projection"
	^ (GLMVector direction: self direction left)
		length: self length
]

{ #category : #accessing }
GLMVector >> length [
	^ length value
]

{ #category : #initialization }
GLMVector >> length: aNumber [

	self
		assert: [ aNumber >= 0 ]
		description: [ 'Length must be non-negative' ].

	length := aNumber
]

{ #category : #accessing }
GLMVector >> next [
	"Return a next vector"
	<return: #GLMVector>

	^ next
]

{ #category : #initialization }
GLMVector >> next: aVector [
	next := aVector
]

{ #category : #initialization }
GLMVector >> north [
	direction := North
]

{ #category : #geometry }
GLMVector >> offset [
	<return: #Point>
	^ self direction * self length
]

{ #category : #geometry }
GLMVector >> origin: aPoint breadth: aNumber [
	"Creates a rectangle to the left of a vector"
	<return: #Rectangle>

	^ self direction origin: aPoint length: length breadth: aNumber
]

{ #category : #accessing }
GLMVector >> previous [
	^ previous
]

{ #category : #initialization }
GLMVector >> previous: aVector [
	previous := aVector
]

{ #category : #accessing }
GLMVector >> rightAngle: aVector [
	"Return true if angle between me and a given angle is right"
	^ self direction rightAngle: aVector direction
]

{ #category : #geometry }
GLMVector >> similar: aLength [
	^ (self class direction: self direction)
		length: aLength;
		yourself
]

{ #category : #initialization }
GLMVector >> south [
	direction := South
]

{ #category : #initialization }
GLMVector >> west [
	direction := West
]
