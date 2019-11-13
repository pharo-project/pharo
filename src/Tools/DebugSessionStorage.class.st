Class {
	#name : #DebugSessionStorage,
	#superclass : #Object,
	#classVars : [
		'storage'
	],
	#category : #'Tools-Debugger'
}

{ #category : #accessing }
DebugSessionStorage class >> clear [
	<script>
	"This method should only be used for development purposes. It does not call #terminate on the stored debug sessions, and therefore does not let the debugged process be removed from the system (they will linger on)"
	self storage removeAll.
]

{ #category : #accessor }
DebugSessionStorage class >> storage [
	storage ifNil: [ storage := OrderedCollection new ].
	^ storage
]

{ #category : #accessing }
DebugSessionStorage class >> storeDebugSession: aDebugSession [
	self storage add: (DebugSessionStorageUnit newFrom: aDebugSession)
]

{ #category : #accessing }
DebugSessionStorage class >> terminate: aDebugSessionStorageUnit [
	"Terminating the debug session in addition to removing it from the storage is necessary to ensure the debugged process is destroyed and does not linger in the image"
	self storage remove: aDebugSessionStorageUnit.
	aDebugSessionStorageUnit debugSession terminate.
]

{ #category : #accessing }
DebugSessionStorage class >> terminateAll [
	<script>
	self storage do: [ :aDebugSessionStorageUnit | self terminate: aDebugSessionStorageUnit ]
]
