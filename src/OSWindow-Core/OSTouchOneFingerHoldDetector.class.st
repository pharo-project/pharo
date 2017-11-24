"
I am the detector of the hold with one finger.
I send these event back to my handler: 
	- OSTouchOneFingerHoldEvent
"
Class {
	#name : #OSTouchOneFingerHoldDetector,
	#superclass : #OSTouchHoldDetector,
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #creation }
OSTouchOneFingerHoldDetector >> createHoldEvent [
	^ OSTouchOneFingerHoldEvent new 
		position: self centerOfAllFingers;
		timestamp: eventHandler lastUpdateTimestamp ;
		deviceId: deviceId;
		yourself
]

{ #category : #preconditions }
OSTouchOneFingerHoldDetector >> necessaryFingers [
	^ 1
]
