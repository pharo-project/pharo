Class {
	#name : #DebugSessionStorage,
	#superclass : #Object,
	#classVars : [
		'storage',
		'subscribersOnStorageUpdate'
	],
	#category : #'Tools-Debugger'
}

{ #category : #events }
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

{ #category : #events }
DebugSessionStorage class >> notifySubscribersOfAStorageUpdate [
	self subscribersOnStorageUpdate do: [ :subscriber | subscriber onDebugSessionStorageUpdate ]
]

{ #category : #events }
DebugSessionStorage class >> removeSubscriberOnStorageUpdate: anObject [
	self subscribersOnStorageUpdate remove: anObject
]

{ #category : #accessor }
DebugSessionStorage class >> storage [
	storage ifNil: [ storage := OrderedCollection new ].
	^ storage
]

{ #category : #accessing }
DebugSessionStorage class >> storeDebugSession: aDebugSession [
	self storage add: (DebugSessionStorageUnit newFrom: aDebugSession).
	self notifySubscribersOfAStorageUpdate.
]

{ #category : #accessing }
DebugSessionStorage class >> subscribersOnStorageUpdate [
	subscribersOnStorageUpdate ifNil: [ subscribersOnStorageUpdate := OrderedCollection new ].
	^ subscribersOnStorageUpdate
]

{ #category : #accessing }
DebugSessionStorage class >> terminate: aDebugSessionStorageUnit [
	"Terminating the debug session in addition to removing it from the storage is necessary to ensure the debugged process is destroyed and does not linger in the image"
	self storage remove: aDebugSessionStorageUnit.
	aDebugSessionStorageUnit debugSession terminate.
	self notifySubscribersOfAStorageUpdate 
]

{ #category : #accessing }
DebugSessionStorage class >> terminateAll [
	<script>
	self storage copy do: [ :aDebugSessionStorageUnit | self terminate: aDebugSessionStorageUnit ]
]
