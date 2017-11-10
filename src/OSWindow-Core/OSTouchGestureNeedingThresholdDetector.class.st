"
I am a gesture detector for gestures who has to travel a certain distance before being detected
"
Class {
	#name : #OSTouchGestureNeedingThresholdDetector,
	#superclass : #OSTouchStateGestureDetector,
	#instVars : [
		'minDistBeforeDetection'
	],
	#classVars : [
		'MinDistanceBeforeDetection'
	],
	#category : #OSWindow-Core-Gestures
}

{ #category : #'class initialization' }
OSTouchGestureNeedingThresholdDetector class >> initialize [
	super initialize.
	MinDistanceBeforeDetection := 0.05
]

{ #category : #accessing }
OSTouchGestureNeedingThresholdDetector class >> minDistanceBeforeDetection [
	"I am the minimum percentage of the touch device that the user have to travel before the beginning of the event"

	^ MinDistanceBeforeDetection
]

{ #category : #accessing }
OSTouchGestureNeedingThresholdDetector class >> minDistanceBeforeDetection: anObject [
	MinDistanceBeforeDetection := anObject
]

{ #category : #preconditions }
OSTouchGestureNeedingThresholdDetector >> beginConditions [
	^ super beginConditions and: [ self eachFingersHadTravelledEnough ]
]

{ #category : #calculating }
OSTouchGestureNeedingThresholdDetector >> distanceTravelledForEachFingers [
	^ self activeFingers
			collect: [ :finger | finger distanceTravelled ]
]

{ #category : #preconditions }
OSTouchGestureNeedingThresholdDetector >> eachFingersHadTravelledEnough [
	^ self distanceTravelledForEachFingers allSatisfy: [ :each | each > minDistBeforeDetection ]
]

{ #category : #initialization }
OSTouchGestureNeedingThresholdDetector >> initialize [
	super initialize.
	minDistBeforeDetection := self class minDistanceBeforeDetection.
]
