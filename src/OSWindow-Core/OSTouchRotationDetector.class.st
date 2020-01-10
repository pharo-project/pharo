"
I am the abstract class for all the Rotation gesture detectors
"
Class {
	#name : #OSTouchRotationDetector,
	#superclass : #OSTouchStateGestureDetector,
	#instVars : [
		'minAngleBeforeDetection',
		'vectorAtBeginnig',
		'lastAngle',
		'currentAngle'
	],
	#classVars : [
		'MinAngleBeforeDetection'
	],
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #accessing }
OSTouchRotationDetector class >> minAngleBeforeDetection [
	"I am the minimum angle, in degrees, required before the beginning of the rotation gesture"

	^ MinAngleBeforeDetection ifNil: [ 10 ]
]

{ #category : #accessing }
OSTouchRotationDetector class >> minAngleBeforeDetection: anInteger [
	MinAngleBeforeDetection := anInteger
]

{ #category : #settings }
OSTouchRotationDetector class >> minAngleBeforeDetectionSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #minAngleBeforeDetection)
		target: self;
		default: 10;
		label: 'Minimal angle (degrees)';
		description:
				'Minimal angle, in degrees, that the fingers have to rotate before detection';
		parent: #rotationGesture
]

{ #category : #settings }
OSTouchRotationDetector class >> touchRotationSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #rotationGesture)
		label: 'Rotation';
		parent: #touchGestures
]

{ #category : #preconditions }
OSTouchRotationDetector >> beginConditions [
	^ super beginConditions and: [ self isBrinkAngleOverwhelmed ]
]

{ #category : #factory }
OSTouchRotationDetector >> createEventBeginning [
	vectorAtBeginnig := self vectorBetweenFirstAndLastFingers.
	currentAngle := vectorAtBeginnig angleWith: self vectorBetweenFirstAndLastFingers.
	self updateCurrentAngle .
	
	^ self eventClassBeginGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dTheta: 0;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself
]

{ #category : #factory }
OSTouchRotationDetector >> createEventDuringMove [
	^ self eventClassMoveGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dTheta: currentAngle - lastAngle;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself
]

{ #category : #factory }
OSTouchRotationDetector >> createEventEnding [
	^ self eventClassEndGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dTheta: currentAngle - lastAngle;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself
]

{ #category : #abstract }
OSTouchRotationDetector >> eventClassBeginGesture [
	self subclassResponsibility
]

{ #category : #abstract }
OSTouchRotationDetector >> eventClassEndGesture [
	self subclassResponsibility
]

{ #category : #abstract }
OSTouchRotationDetector >> eventClassMoveGesture [
	self subclassResponsibility
]

{ #category : #initialization }
OSTouchRotationDetector >> initialize [
	super initialize.
	currentAngle := 0.0.
	minAngleBeforeDetection := self class minAngleBeforeDetection degreesToRadians
]

{ #category : #preconditions }
OSTouchRotationDetector >> isBrinkAngleOverwhelmed [
	^ (currentAngle > (2 * Float pi - minAngleBeforeDetection) or: [ currentAngle < minAngleBeforeDetection ]) not
]

{ #category : #update }
OSTouchRotationDetector >> updateCurrentAngle [
	lastAngle := currentAngle.
	currentAngle := vectorAtBeginnig angleWith: self vectorBetweenFirstAndLastFingers.
]

{ #category : #calculating }
OSTouchRotationDetector >> vectorBetweenFirstAndLastFingers [
	^ self activeFingers first lastPosition - self activeFingers last lastPosition
]

{ #category : #visiting }
OSTouchRotationDetector >> visitTouchPointerDownEvent: anEvent [
	super visitTouchPointerDownEvent: anEvent.
	eventHandler fingerCount = self necessaryFingers 
		ifTrue:[ vectorAtBeginnig := self vectorBetweenFirstAndLastFingers ]
]

{ #category : #visiting }
OSTouchRotationDetector >> visitTouchPointerMoveEvent: anEvent [
	super visitTouchPointerMoveEvent: anEvent.
	eventHandler fingerCount = self necessaryFingers 
		ifTrue:[ self updateCurrentAngle ]
]
