Class {
	#name : #EventListenerForDebugSessionStorageTest,
	#superclass : #Object,
	#instVars : [
		'nbOfDebugSessionStorageLimitUpdateEventReceived',
		'nbOfDebugSessionStorageUpdateEventReceived'
	],
	#category : #'Tools-Test-Debugger'
}

{ #category : #'react to events' }
EventListenerForDebugSessionStorageTest >> initialize [
	nbOfDebugSessionStorageLimitUpdateEventReceived := 0.
	nbOfDebugSessionStorageUpdateEventReceived := 0
]

{ #category : #accessing }
EventListenerForDebugSessionStorageTest >> nbOfDebugSessionStorageLimitUpdateEventReceived [
	^ nbOfDebugSessionStorageLimitUpdateEventReceived
]

{ #category : #accessing }
EventListenerForDebugSessionStorageTest >> nbOfDebugSessionStorageUpdateEventReceived [
	^ nbOfDebugSessionStorageUpdateEventReceived
]

{ #category : #'react to events' }
EventListenerForDebugSessionStorageTest >> onDebugSessionStorageLimitUpdate [
	nbOfDebugSessionStorageLimitUpdateEventReceived := nbOfDebugSessionStorageLimitUpdateEventReceived + 1
]

{ #category : #'react to events' }
EventListenerForDebugSessionStorageTest >> onDebugSessionStorageUpdate [
	nbOfDebugSessionStorageUpdateEventReceived := nbOfDebugSessionStorageUpdateEventReceived + 1
]
