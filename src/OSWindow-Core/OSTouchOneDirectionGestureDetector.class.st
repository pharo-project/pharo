"
I detect gestures which needs all fingers to go on the same direction
"
Class {
	#name : #OSTouchOneDirectionGestureDetector,
	#superclass : #OSTouchDirectionnalGestureDetector,
	#instVars : [
		'direction'
	],
	#category : #OSWindow-Core-Gestures
}

{ #category : #preconditions }
OSTouchOneDirectionGestureDetector >> allFingersGoesInTheSameDirection [
	"We suppose that the direction of the gesture is the same as the first finger"
	direction := self activeFingers first direction.
	
	^ activeFingers allSatisfy: [ :finger | finger direction = direction ]
]

{ #category : #preconditions }
OSTouchOneDirectionGestureDetector >> beginConditions [
	^ super beginConditions and: [ self allFingersGoesInTheSameDirection ]
]

{ #category : #'event field' }
OSTouchOneDirectionGestureDetector >> dDist [
	| directionDDist |
	directionDDist := (self activeFingers collect: [ :each | each delta ])
		average * direction abs.
	^ directionDDist x + directionDDist y
]

{ #category : #accessing }
OSTouchOneDirectionGestureDetector >> direction [
	^ direction
]

{ #category : #accessing }
OSTouchOneDirectionGestureDetector >> direction: anObject [
	direction := anObject
]
