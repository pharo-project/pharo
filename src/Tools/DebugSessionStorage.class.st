"
I store debug sessions (see class DebugSession) when they are created because an exception was signalled but not caught.
I implement the singleton design pattern. My singleton can be obtained by executing `DebugSessionStorage singleton`.
Upon storing a debug session, I wrap it in a new DebugSessionStorageUnit, to also store the time at which the session was stored.
There is a limit to how many debug sessions I keep, defined by my @storageLimitForDebugSessionsNotKeptAlive variable. Oldest debug sessions are removed first. I always keep debug sessions that are currently open in a debugger (see DebugSession>>#keepAlive:), and they do not count towards the storage limit.

# Variables
- storage  |  OrderedCollection<DebugSessionStorageUnit>
	The list in which debug sessions are stored
- storageLimitForDebugSessionsNotKeptAlive  |  Integer
	How many debug sessions not currently open in a debugger should I keep
- subscribersOnStorageLimitUpdate  |  WeakOrderedCollection<Object>
- subscribersOnStorageUpdate  |  WeakOrderedCollection<Object>

# Long Description
**Description**
When a DebugSession is created (typically when an exception is signaled but not caught, before it is debugged), `DebugSessionStorage` stores it. To avoid cluttering the image with tons of processes (because storing a DebugSession means keeping the process it is debugging), `DebugSessionStorage` has a (configurable) limit on how many DebugSessions it can hold. When the limit is exceeded, the oldest DebugSessions are terminated (along with the process they debug) to make room. The exception is if a debugger is currently open on a DebugSession. A DebugSession currently opened in a debugger is not terminated, and does not count towards the storage limit. It will go back to counting towards the limit if the debugger in which it was opened is closed.

**The Point**
This allows to reopen previously closed DebugSessions, and to do so with a different debugger. The ability to open DebugSessions with a different debugger already exists in some fashion, but it is internal to the GTDebugger (you can see it at play if you click ""Bytecode"" in the top right corner of the GTDebugger, it will switch the debugger to the GTBytecodeDebugger), so it is not easy to support new non-GT debuggers.

**Additional Detail on DebugSessions life-cycle**
To maintain a DebugSession alive, a debugger has to call `#keepAlive:` on it (with itself as argument). To release the DebugSession, it has to call `#stopKeepingAlive:` (again, with itself as argument).
"
Class {
	#name : #DebugSessionStorage,
	#superclass : #Object,
	#instVars : [
		'storage',
		'storageLimitForDebugSessionsNotKeptAlive',
		'subscribersOnStorageLimitUpdate',
		'subscribersOnStorageUpdate'
	],
	#classVars : [
		'singleton'
	],
	#category : #'Tools-Debugger'
}

{ #category : #singleton }
DebugSessionStorage class >> singleton [
	singleton ifNil: [ singleton := self new ].
	^ singleton
]

{ #category : #'publish events' }
DebugSessionStorage >> addSubscriberOnStorageLimitUpdate: anObject [
	self subscribersOnStorageLimitUpdate add: anObject
]

{ #category : #'publish events' }
DebugSessionStorage >> addSubscriberOnStorageUpdate: anObject [
	self subscribersOnStorageUpdate add: anObject
]

{ #category : #private }
DebugSessionStorage >> clear [
	<script>
	"This method should only be used for development purposes. It does not call #terminate on the stored debug sessions, and therefore does not let the debugged process be removed from the system (they will linger on)"
	self storage removeAll.
	self notifySubscribersOfAStorageUpdate.
]

{ #category : #storage }
DebugSessionStorage >> enforceStorageLimit [
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
DebugSessionStorage >> notifySubscribersOfAStorageLimitUpdate [
	self subscribersOnStorageLimitUpdate do: [ :subscriber_ | subscriber_ ifNotNil: [:subscriber | subscriber onDebugSessionStorageLimitUpdate ]]
]

{ #category : #'publish events' }
DebugSessionStorage >> notifySubscribersOfAStorageUpdate [
	self subscribersOnStorageUpdate do: [ :subscriber_ | subscriber_ ifNotNil: [:subscriber | subscriber onDebugSessionStorageUpdate ]]
]

{ #category : #'react to events' }
DebugSessionStorage >> onDebugSession: aDebugSession gettingKeptAliveBy: anObject [
	self notifySubscribersOfAStorageUpdate 
]

{ #category : #'react to events' }
DebugSessionStorage >> onDebugSession: aDebugSession stoppingGettingKeptAliveBy: anObject [
	self notifySubscribersOfAStorageUpdate.
	aDebugSession shouldBeKeptAlive ifFalse: [ self enforceStorageLimit ] "If @aDebugSession is no longer kept alive as a result, maybe we are now breaking the storage limit rule and need to enforce it again"
]

{ #category : #'publish events' }
DebugSessionStorage >> removeSubscriberOnStorageLimitUpdate: anObject [
	self subscribersOnStorageLimitUpdate remove: anObject ifAbsent: [ "It's a weak collection, its elements can be nilled by the garbage collector at any time if they are not referenced elsewhere, so it can happen not to find the object to remove because it has already been nilled" ]
]

{ #category : #'publish events' }
DebugSessionStorage >> removeSubscriberOnStorageUpdate: anObject [
	self subscribersOnStorageUpdate remove: anObject ifAbsent: [ "It's a weak collection, its elements can be nilled by the garbage collector at any time if they are not referenced elsewhere, so it can happen not to find the object to remove because it has already been nilled" ]
]

{ #category : #accessing }
DebugSessionStorage >> storage [
	storage ifNil: [ storage := OrderedCollection new ].
	^ storage
]

{ #category : #accessing }
DebugSessionStorage >> storageLimitForDebugSessionsNotKeptAlive [
	storageLimitForDebugSessionsNotKeptAlive ifNil: [ storageLimitForDebugSessionsNotKeptAlive := 5 ].
	^ storageLimitForDebugSessionsNotKeptAlive
]

{ #category : #accessing }
DebugSessionStorage >> storageLimitForDebugSessionsNotKeptAlive: anInteger [
	(anInteger isInteger not | anInteger <= 0) ifTrue: [ InvalidValueForStorageLimit signal ].
	storageLimitForDebugSessionsNotKeptAlive := anInteger.
	self notifySubscribersOfAStorageLimitUpdate.
	self enforceStorageLimit
]

{ #category : #storage }
DebugSessionStorage >> storeDebugSession: aDebugSession [
	| storageUnit |
	storageUnit := DebugSessionStorageUnit newFrom: aDebugSession.
	self storage add: storageUnit.
	aDebugSession addSubscriberToKeepAliveEvents: self.
	self notifySubscribersOfAStorageUpdate.
	self enforceStorageLimit.
	^ storageUnit
]

{ #category : #accessing }
DebugSessionStorage >> subscribersOnStorageLimitUpdate [
	subscribersOnStorageLimitUpdate ifNil: [ subscribersOnStorageLimitUpdate := WeakOrderedCollection new ].
	^ subscribersOnStorageLimitUpdate
]

{ #category : #accessing }
DebugSessionStorage >> subscribersOnStorageUpdate [
	subscribersOnStorageUpdate ifNil: [ subscribersOnStorageUpdate := WeakOrderedCollection new ].
	^ subscribersOnStorageUpdate
]

{ #category : #termination }
DebugSessionStorage >> terminate: aDebugSessionStorageUnit [
	"See comment of method #_NoNotification:"
	self terminate_NoNotification: aDebugSessionStorageUnit.
	self notifySubscribersOfAStorageUpdate 
]

{ #category : #termination }
DebugSessionStorage >> terminateAll [
	<script>
	self storage copy do: [ :aDebugSessionStorageUnit | self terminate: aDebugSessionStorageUnit ]
]

{ #category : #termination }
DebugSessionStorage >> terminate_NoNotification: aDebugSessionStorageUnit [
	"Terminating the debug session in addition to removing it from the storage is necessary to ensure the debugged process is destroyed and does not linger in the image"
	self storage remove: aDebugSessionStorageUnit.
	aDebugSessionStorageUnit debugSession terminate.
]
