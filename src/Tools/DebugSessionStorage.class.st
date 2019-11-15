Class {
	#name : #DebugSessionStorage,
	#superclass : #Object,
	#classVars : [
		'storage',
		'storageLimitForDebugSessionsNotKeptAlive',
		'subscribersOnStorageLimitUpdate',
		'subscribersOnStorageUpdate'
	],
	#category : #'Tools-Debugger'
}

{ #category : #'publish events' }
DebugSessionStorage class >> addSubscriberOnStorageLimitUpdate: anObject [
	self subscribersOnStorageLimitUpdate add: anObject
]

{ #category : #'publish events' }
DebugSessionStorage class >> addSubscriberOnStorageUpdate: anObject [
	self subscribersOnStorageUpdate add: anObject
]

{ #category : #private }
DebugSessionStorage class >> clear [
	<script>
	"This method should only be used for development purposes. It does not call #terminate on the stored debug sessions, and therefore does not let the debugged process be removed from the system (they will linger on)"
	self storage removeAll.
	self notifySubscribersOfAStorageUpdate.
]

{ #category : #cleanup }
DebugSessionStorage class >> enforceStorageLimit [
	"Terminate stored debug sessions until the limit (defined in the storageLimitForDebugSessionNotKeptAlive variable) is satisfied.
	Oldest debug sessions are terminated first.
	Debug sessions that are being kept alive (see DebugSession>>#keepAlive:) are ignored and remain stored regardless. They do not count towards the storage limit."
	| debugSessionStorageUnits |
	"Smalltalk garbageCollect." "This is to make sure that closed debuggers are garbage collected so that they stop keeping alive their debug sessions. Doing a gabage collect here is maybe a bit heavy-handed. There maybe is a better solution"
	debugSessionStorageUnits := self storage reject: [ :aDebugSessionStorageUnit | aDebugSessionStorageUnit debugSession shouldBeKeptAlive ]. "Select all the stored debug sessions that are not being kept alive"
	debugSessionStorageUnits := debugSessionStorageUnits sort: [:el1 :el2| el1 timeOfCreation <= el2 timeOfCreation]. "Sort the debug sessions from oldest to newest"
	1 to: (debugSessionStorageUnits size - self storageLimitForDebugSessionsNotKeptAlive) do: [ :i | self terminate_NoNotification: (debugSessionStorageUnits at: i) ].
	self notifySubscribersOfAStorageUpdate.
	
]

{ #category : #'publish events' }
DebugSessionStorage class >> notifySubscribersOfAStorageLimitUpdate [
	self subscribersOnStorageLimitUpdate do: [ :subscriber_ | subscriber_ ifNotNil: [:subscriber | subscriber onDebugSessionStorageLimitUpdate ]]
]

{ #category : #'publish events' }
DebugSessionStorage class >> notifySubscribersOfAStorageUpdate [
	self subscribersOnStorageUpdate do: [ :subscriber_ | subscriber_ ifNotNil: [:subscriber | subscriber onDebugSessionStorageUpdate ]]
]

{ #category : #'react to events' }
DebugSessionStorage class >> onDebugSession: aDebugSession gettingKeptAliveBy: anObject [
	self notifySubscribersOfAStorageUpdate 
]

{ #category : #'react to events' }
DebugSessionStorage class >> onDebugSession: aDebugSession stoppingGettingKeptAliveBy: anObject [
	self notifySubscribersOfAStorageUpdate.
	aDebugSession shouldBeKeptAlive ifFalse: [ self enforceStorageLimit ] "If @aDebugSession is no longer kept alive as a result, maybe we are now breaking the storage limit rule and need to enforce it again"
]

{ #category : #'publish events' }
DebugSessionStorage class >> removeSubscriberOnStorageLimitUpdate: anObject [
	self subscribersOnStorageLimitUpdate remove: anObject
]

{ #category : #'publish events' }
DebugSessionStorage class >> removeSubscriberOnStorageUpdate: anObject [
	self subscribersOnStorageUpdate remove: anObject
]

{ #category : #accessing }
DebugSessionStorage class >> storage [
	storage ifNil: [ storage := OrderedCollection new ].
	^ storage
]

{ #category : #accessing }
DebugSessionStorage class >> storageLimitForDebugSessionsNotKeptAlive [
	storageLimitForDebugSessionsNotKeptAlive ifNil: [ storageLimitForDebugSessionsNotKeptAlive := 5 ].
	^ storageLimitForDebugSessionsNotKeptAlive
]

{ #category : #accessing }
DebugSessionStorage class >> storageLimitForDebugSessionsNotKeptAlive: anInteger [
	(anInteger isInteger not | anInteger <= 0) ifTrue: [ InvalidValueForStorageLimit signal ].
	storageLimitForDebugSessionsNotKeptAlive := anInteger
]

{ #category : #accessing }
DebugSessionStorage class >> storeDebugSession: aDebugSession [
	self storage add: (DebugSessionStorageUnit newFrom: aDebugSession).
	aDebugSession addSubscriberToKeepAliveEvents: self.
	self notifySubscribersOfAStorageUpdate.
]

{ #category : #accessing }
DebugSessionStorage class >> subscribersOnStorageLimitUpdate [
	subscribersOnStorageLimitUpdate ifNil: [ subscribersOnStorageLimitUpdate := WeakOrderedCollection new ].
	^ subscribersOnStorageLimitUpdate
]

{ #category : #accessing }
DebugSessionStorage class >> subscribersOnStorageUpdate [
	subscribersOnStorageUpdate ifNil: [ subscribersOnStorageUpdate := WeakOrderedCollection new ].
	^ subscribersOnStorageUpdate
]

{ #category : #accessing }
DebugSessionStorage class >> terminate: aDebugSessionStorageUnit [
	"See comment of method #_NoNotification:"
	self terminate_NoNotification: aDebugSessionStorageUnit.
	self notifySubscribersOfAStorageUpdate 
]

{ #category : #accessing }
DebugSessionStorage class >> terminateAll [
	<script>
	self storage copy do: [ :aDebugSessionStorageUnit | self terminate: aDebugSessionStorageUnit ]
]

{ #category : #private }
DebugSessionStorage class >> terminate_NoNotification: aDebugSessionStorageUnit [
	"Terminating the debug session in addition to removing it from the storage is necessary to ensure the debugged process is destroyed and does not linger in the image"
	self storage remove: aDebugSessionStorageUnit.
	aDebugSessionStorageUnit debugSession terminate.
]
