"
Instances of this class are used to store debug sessions by DebugSessionStorage. Their main feature is that they also store the time at which they were created.
"
Class {
	#name : #DebugSessionStorageUnit,
	#superclass : #Object,
	#instVars : [
		'debugSession',
		'timeOfCreation'
	],
	#category : #'Tools-Debugger'
}

{ #category : #'instance creation' }
DebugSessionStorageUnit class >> newFrom: aDebugSession [
	^ self new
		debugSession: aDebugSession;
		timeOfCreation: DateAndTime now;
		yourself.
]

{ #category : #accessing }
DebugSessionStorageUnit >> debugSession [
	^ debugSession
]

{ #category : #accessing }
DebugSessionStorageUnit >> debugSession: anObject [
	debugSession := anObject
]

{ #category : #accessing }
DebugSessionStorageUnit >> timeOfCreation [
	^ timeOfCreation
]

{ #category : #accessing }
DebugSessionStorageUnit >> timeOfCreation: anObject [
	timeOfCreation := anObject
]
