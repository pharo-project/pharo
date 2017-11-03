"
I am recording all movements and informations from a finger
"
Class {
	#name : #OSTouchFingerTracker,
	#superclass : #Object,
	#instVars : [
		'fingerId',
		'deviceId',
		'delta',
		'pressure',
		'timestamp',
		'move',
		'firstPosition',
		'lastPosition'
	],
	#category : #OSWindow-Core-Gestures
}

{ #category : #'instance creation' }
OSTouchFingerTracker class >> for: anEvent [
	^ self new deviceId: anEvent deviceId;
		fingerId: anEvent fingerId;
		delta: anEvent delta;
		firstPosition: anEvent position;
		pressure: anEvent pressure;
		timestamp: anEvent timestamp
]

{ #category : #moves }
OSTouchFingerTracker >> addToMove: aPoint [
	move add: aPoint.
	lastPosition := aPoint
	
]

{ #category : #accessing }
OSTouchFingerTracker >> delta [
	^ delta
]

{ #category : #accessing }
OSTouchFingerTracker >> delta: anObject [
	delta := anObject
]

{ #category : #accessing }
OSTouchFingerTracker >> deviceId [
	^ deviceId
]

{ #category : #accessing }
OSTouchFingerTracker >> deviceId: anObject [
	deviceId := anObject
]

{ #category : #calculating }
OSTouchFingerTracker >> direction [
	"Only returns noth, east, west, south"
	^ delta x abs > delta y abs
		ifTrue: [ delta x sign @ 0 ]
		ifFalse: [ 0 @ delta y sign ]
]

{ #category : #calculating }
OSTouchFingerTracker >> distanceFromLineFormedByFinger: aFingerTracker [
	| otherLast |
	otherLast := aFingerTracker lastPosition.

	^ lastPosition distanceTo: 
		(lastPosition nearestPointAlongLineFrom: 
			otherLast + aFingerTracker delta 
		to:
			otherLast)
]

{ #category : #calculating }
OSTouchFingerTracker >> distanceTravelled [
	^ firstPosition distanceTo: lastPosition
]

{ #category : #accessing }
OSTouchFingerTracker >> fingerId [
	^ fingerId
]

{ #category : #accessing }
OSTouchFingerTracker >> fingerId: anObject [
	fingerId := anObject
]

{ #category : #moves }
OSTouchFingerTracker >> firstPosition [
	^ firstPosition
]

{ #category : #moves }
OSTouchFingerTracker >> firstPosition: anObject [
	move removeAll.
	firstPosition := anObject.
	self addToMove: firstPosition.
]

{ #category : #initialization }
OSTouchFingerTracker >> initialize [
	move := OrderedCollection new.
]

{ #category : #calculating }
OSTouchFingerTracker >> isGettingCloserToPoint: aPoint [
	| rectangle |
	rectangle := lastPosition rectangle: aPoint.
	^ (rectangle containsPoint: lastPosition + delta)
]

{ #category : #moves }
OSTouchFingerTracker >> lastPosition [
	^ lastPosition
]

{ #category : #moves }
OSTouchFingerTracker >> lastPosition: anObject [
	lastPosition := anObject
]

{ #category : #accessing }
OSTouchFingerTracker >> move [
	^ move
]

{ #category : #accessing }
OSTouchFingerTracker >> move: anObject [
	move := anObject
]

{ #category : #accessing }
OSTouchFingerTracker >> pressure [
	^ pressure
]

{ #category : #accessing }
OSTouchFingerTracker >> pressure: anObject [
	pressure := anObject
]

{ #category : #accessing }
OSTouchFingerTracker >> timestamp [
	^ timestamp 
]

{ #category : #accessing }
OSTouchFingerTracker >> timestamp: anObject [
	timestamp := anObject
]

{ #category : #calculating }
OSTouchFingerTracker >> totalDistanceTravelled [
	| previous sum |
	previous := firstPosition.
	sum := 0.
	move
		do: [ :position | 
			sum := sum + position distanceTo: firstPosition.
			previous := position ].
	^ sum
]

{ #category : #updating }
OSTouchFingerTracker >> updateDelta: aPoint [
	delta := (delta + (2 * aPoint)) / 3
]

{ #category : #updating }
OSTouchFingerTracker >> updateWithEvent: anEvent [
	self updateDelta: anEvent delta;
		addToMove: anEvent position;
		pressure: anEvent pressure;
		timestamp: anEvent timestamp
]
