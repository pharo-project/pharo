"
I detect gestures which needs all fingers to go on a certain direction (creating a delta distance)
"
Class {
	#name : #OSTouchDirectionnalGestureDetector,
	#superclass : #OSTouchGestureNeedingThresholdDetector,
	#category : #OSWindow-Core-Gestures
}

{ #category : #'event field' }
OSTouchDirectionnalGestureDetector >> dDist [
	^ self subclassResponsibility
]
