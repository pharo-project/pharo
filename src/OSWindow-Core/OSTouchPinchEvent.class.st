"
Superclass of all pinches events
"
Class {
	#name : #OSTouchPinchEvent,
	#superclass : #OSTouchComplexEvent,
	#instVars : [
		'dDist'
	],
	#category : #'OSWindow-Core-Events-Touch'
}

{ #category : #'as yet unclassified' }
OSTouchPinchEvent class >> id [
	^ #pinch
]

{ #category : #accessing }
OSTouchPinchEvent >> dDist [
	^ dDist
]

{ #category : #accessing }
OSTouchPinchEvent >> dDist: anObject [
	dDist := anObject
]
