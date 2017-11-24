"
I am an abstract class for the gestures detector, to work I have to register with an OSWindowGestureHandler
"
Class {
	#name : #OSTouchGestureDetector,
	#superclass : #OSWindowEventGestureVisitor,
	#instVars : [
		'deviceId',
		'activeFingers',
		'eventHandler'
	],
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #'instance creation' }
OSTouchGestureDetector class >> onDeviceId: anInteger [
	^ self new deviceId: anInteger
]

{ #category : #initialization }
OSTouchGestureDetector class >> reinitializeAllSubclasses [
	"self reinitializeAllSubclasses"
	self allSubclassesDo: [ :each | each initialize ]
]

{ #category : #settings }
OSTouchGestureDetector class >> touchGestureSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #touchGestures)
		parent: #appearance;
		label: 'Touch Gestures'
]

{ #category : #accessing }
OSTouchGestureDetector >> activeFingers [
	^ activeFingers
]

{ #category : #accessing }
OSTouchGestureDetector >> activeFingers: anObject [
	activeFingers := anObject
]

{ #category : #factory }
OSTouchGestureDetector >> centerOfAllFingers [
	| fingersPosition sum cpt |
	cpt := 0.
	sum := 0@0.
	
	fingersPosition := self activeFingers
		collect: [ :fingerTracker | fingerTracker lastPosition ].
		
	fingersPosition ifEmpty: [ ^ nil ].
	
	fingersPosition do: [ :point | sum := sum + point. cpt := cpt + 1 ].
	
	^ sum / cpt
]

{ #category : #accessing }
OSTouchGestureDetector >> deviceId [
	^ deviceId
]

{ #category : #accessing }
OSTouchGestureDetector >> deviceId: anInteger [
	deviceId := anInteger
]

{ #category : #accessing }
OSTouchGestureDetector >> eventHandler [
	^ eventHandler
]

{ #category : #accessing }
OSTouchGestureDetector >> eventHandler: anObject [
	eventHandler := anObject
]

{ #category : #factory }
OSTouchGestureDetector >> sendToEventHandler: anEvent [
	eventHandler handleGestureEvent: anEvent
]
