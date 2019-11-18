Class {
	#name : #DebugSessionStorageTest,
	#superclass : #TestCase,
	#instVars : [
		'debugSessionStorage',
		'eventListener',
		'debugSession',
		'debugSession2',
		'debugSession3'
	],
	#category : #'Tools-Test-Debugger'
}

{ #category : #running }
DebugSessionStorageTest >> createDebugSessionForTesting [
	| dbSession process |
	process := [ 'hello'] fork.
	dbSession :=  process newDebugSessionNamed: 'forTest' startedAt: process suspendedContext.
	dbSession := DebugSessionDecoratorForTest newOn: dbSession.
	^ dbSession
]

{ #category : #running }
DebugSessionStorageTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	debugSession := self createDebugSessionForTesting.
	debugSession2 := self createDebugSessionForTesting.
	debugSession3 := self createDebugSessionForTesting.
	debugSessionStorage := DebugSessionStorage new.
	eventListener := EventListenerForDebugSessionStorageTest new.
	debugSessionStorage addSubscriberOnStorageLimitUpdate: eventListener.
	debugSessionStorage addSubscriberOnStorageUpdate: eventListener

]

{ #category : #tests }
DebugSessionStorageTest >> testEnforceStorageLimit [
	"Setting the storage limit to 1, and adding 3 debug sessions: 1, 2 and 3. 2 is being kept alive to simulate a debugger being open on it."
	| storageUnit storageUnit2 storageUnit3 |
	debugSessionStorage storageLimitForDebugSessionsNotKeptAlive: 1.
	debugSession2 keepAlive: self.
	storageUnit := debugSessionStorage storeDebugSession: debugSession.
	storageUnit2 := debugSessionStorage storeDebugSession: debugSession2.
	storageUnit3 := debugSessionStorage storeDebugSession: debugSession3.
	self assert: debugSessionStorage storage size equals: 2. "After adding session 3, session 1 must have been terminated, as it is the oldest session stored that is not being kept alive"
	self assert: (debugSessionStorage storage includes: storageUnit2).
	self assert: (debugSessionStorage storage includes: storageUnit3).
	debugSession2 stopKeepingAlive: self.
	self assert: debugSessionStorage storage size equals: 1. "After we stop keeping alive session 2, it should also be removed from the storage"
	self assert: (debugSessionStorage storage includes: storageUnit2) not.
	self assert: (debugSessionStorage storage includes: storageUnit3)
]

{ #category : #tests }
DebugSessionStorageTest >> testStorageLimitForDebugSessionsNotKeptAlive [
	"Check that the storage limit is enforced after being lowered, and that listeners do get the event that the storage limit was changed"
	| storageUnit storageUnit2 |
	debugSessionStorage storageLimitForDebugSessionsNotKeptAlive: 2.
	storageUnit := debugSessionStorage storeDebugSession: debugSession.
	storageUnit2 := debugSessionStorage storeDebugSession: debugSession2.
	self assert: debugSessionStorage storage size equals: 2.
	debugSessionStorage storageLimitForDebugSessionsNotKeptAlive: 1.
	self assert: (debugSessionStorage storage includes: storageUnit) not.
	self assert: (debugSessionStorage storage includes: storageUnit2
	).
	self assert: eventListener nbOfDebugSessionStorageLimitUpdateEventReceived equals: 2
]

{ #category : #tests }
DebugSessionStorageTest >> testStoreDebugSession [
	| classSetDebugSessionStorageUnitAndSubclasses |
	self assert: debugSessionStorage storage size equals: 0.
	debugSessionStorage storeDebugSession: debugSession.
	self assert: debugSessionStorage storage size equals: 1. "Assert that calling #storeDebugSession: increased the size of storage by 1"
	classSetDebugSessionStorageUnitAndSubclasses := DebugSessionStorageUnit allSubclasses add: DebugSessionStorageUnit; yourself.
	self assert: (classSetDebugSessionStorageUnitAndSubclasses includes: (debugSessionStorage storage at: 1) class). "Assert that the element added to storage is an instance of DebugSessionStorageUnit or one of its subclasses"
	self assert: eventListener nbOfDebugSessionStorageUpdateEventReceived >= 1. "Asserts that listeners of the storageUpdate event were notified by the debugSessionStorage when a debug session was added"
]

{ #category : #tests }
DebugSessionStorageTest >> testTerminate [
	| storageUnit |
	storageUnit := debugSessionStorage storeDebugSession: debugSession.
	debugSessionStorage terminate: storageUnit.
	self assert: debugSessionStorage storage size equals: 0. "Asserts that the storage unit has been removed from storage"
	self assert: debugSession nbOfTimesTerminateWasCalled equals: 1. "Asserts that the debug session was told to terminate the process it was debugging"
	self assert: eventListener nbOfDebugSessionStorageUpdateEventReceived >= 1. "Asserts that listeners of the storageUpdate event were notified by the debugSessionStorage when a debug session was added"
]
