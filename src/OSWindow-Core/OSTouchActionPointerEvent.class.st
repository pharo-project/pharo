"
Superclass of all the pointer Event
	Can be a finger or something else.
"
Class {
	#name : #OSTouchActionPointerEvent,
	#superclass : #OSTouchActionEvent,
	#instVars : [
		'position',
		'delta',
		'pressure',
		'fingerId'
	],
	#category : #OSWindow-Core-Events-Touch
}

{ #category : #accessing }
OSTouchActionPointerEvent >> delta [
	^ delta
]

{ #category : #accessing }
OSTouchActionPointerEvent >> delta: anObject [
	delta := anObject
]

{ #category : #accessing }
OSTouchActionPointerEvent >> fingerId [
	^ fingerId
]

{ #category : #accessing }
OSTouchActionPointerEvent >> fingerId: anObject [
	fingerId := anObject
]

{ #category : #accessing }
OSTouchActionPointerEvent >> position [
	^ position
]

{ #category : #accessing }
OSTouchActionPointerEvent >> position: anObject [
	position := anObject
]

{ #category : #accessing }
OSTouchActionPointerEvent >> pressure [
	^ pressure
]

{ #category : #accessing }
OSTouchActionPointerEvent >> pressure: anObject [
	pressure := anObject
]
