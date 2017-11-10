"
A monitor provides process synchronization that is more high level than the one provided by a Semaphore. Similar to the classical definition of a Monitor it has the following properties:

1) At any time, only one process can execute code inside a critical section of a monitor.
2) A monitor is reentrant, which means that the active process in a monitor never gets blocked when it enters a (nested) critical section of the same monitor.
3) Inside a critical section, a process can wait for an event that may be coupled to a certain condition. If the condition is not fulfilled, the process leaves the monitor temporarily (in order to let other processes enter) and waits until another process signals the event. Then, the original process checks the condition again (this is often necessary because the state of the monitor could have changed in the meantime) and continues if it is fulfilled.
4) The monitor is fair, which means that the process that is waiting on a signaled condition the longest gets activated first.
5) The monitor allows you to define timeouts after which a process gets activated automatically.


Basic usage:

Monitor>>critical: aBlock
Critical section.
Executes aBlock as a critical section. At any time, only one process can execute code in a critical section.
NOTE: All the following synchronization operations are only valid inside the critical section of the monitor!

Monitor>>wait
Unconditional waiting for the default event.
The current process gets blocked and leaves the monitor, which means that the monitor allows another process to execute critical code. When the default event is signaled, the original process is resumed.

Monitor>>waitWhile: aBlock
Conditional waiting for the default event.
The current process gets blocked and leaves the monitor only if the argument block evaluates to true. This means that another process can enter the monitor. When the default event is signaled, the original process is resumed, which means that the condition (argument block) is checked again. Only if it evaluates to false, does execution proceed. Otherwise, the process gets blocked and leaves the monitor again...

Monitor>>waitUntil: aBlock
Conditional waiting for the default event.
See Monitor>>waitWhile: aBlock.

Monitor>>signal
One process waiting for the default event is woken up.

Monitor>>signalAll
All processes waiting for the default event are woken up.


Using non-default (specific) events:

Monitor>>waitFor: aSymbol
Unconditional waiting for the non-default event represented by the argument symbol.
Same as Monitor>>wait, but the process gets only reactivated by the specific event and not the default event.

Monitor>>waitWhile: aBlock for: aSymbol
Confitional waiting for the non-default event represented by the argument symbol.
Same as Monitor>>waitWhile:for:, but the process gets only reactivated by the specific event and not the default event.

Monitor>>waitUntil: aBlock for: aSymbol
Confitional waiting for the non-default event represented by the argument symbol.
See Monitor>>waitWhile:for: aBlock.

Monitor>>signal: aSymbol
One process waiting for the given event is woken up. If there is no process waiting for this specific event, a process waiting for the default event gets resumed.

Monitor>>signalAll: aSymbol
All process waiting for the given event or the default event are woken up.

Monitor>>signalReallyAll
All processes waiting for any events (default or specific) are woken up.


Using timeouts

Monitor>>waitMaxMilliseconds: anInteger
Monitor>>waitFor: aSymbol maxMilliseconds: anInteger
Same as Monitor>>wait (resp. Monitor>>waitFor:), but the process gets automatically woken up when the specified time has passed.

Monitor>>waitWhile: aBlock maxMilliseconds: anInteger
Monitor>>waitWhile: aBlock for: aSymbol maxMilliseconds: anInteger
Same as Monitor>>waitWhile: (resp. Monitor>>waitWhile:for:), but the process gets automatically woken up when the specified time has passed.

Monitor>>waitUntil: aBlock maxMilliseconds: anInteger
Monitor>>waitUntil: aBlock for: aSymbol maxMilliseconds: anInteger
Same as Monitor>>waitUntil: (resp. Monitor>>waitUntil:for:), but the process gets automatically woken up when the specified time has passed.
"
Class {
	#name : #Monitor,
	#superclass : #Object,
	#instVars : [
		'mutex',
		'ownerProcess',
		'nestingLevel',
		'defaultQueue',
		'queueDict',
		'queuesMutex'
	],
	#category : #Kernel-Processes
}

{ #category : #private }
Monitor >> checkOwnerProcess [
	self isOwnerProcess
		ifFalse: [self error: 'Monitor access violation'].
]

{ #category : #accessing }
Monitor >> cleanup [
	self checkOwnerProcess.
	self critical: [self privateCleanup].
]

{ #category : #synchronization }
Monitor >> critical: aBlock [
	"Critical section.
	Executes aBlock as a critical section. At any time, only one process can be executing code 
	in a critical section.
	NOTE: All the following synchronization operations are only valid inside the critical section 
	of the monitor!"

	^[
	self enter.
	aBlock value]
		ensure: [self exit].
]

{ #category : #private }
Monitor >> defaultQueue [
	defaultQueue ifNil: [defaultQueue := OrderedCollection new].
	^ defaultQueue
]

{ #category : #private }
Monitor >> enter [
	self isOwnerProcess ifTrue: [
		nestingLevel := nestingLevel + 1.
	] ifFalse: [
		mutex wait.
		ownerProcess := Processor activeProcess.
		nestingLevel := 1.
	].
]

{ #category : #private }
Monitor >> exit [
	nestingLevel := nestingLevel - 1.
	nestingLevel < 1 ifTrue: [
		ownerProcess := nil.
		mutex signal
	].
]

{ #category : #private }
Monitor >> exitAndWaitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil [
	| lock delay |
	lock := queuesMutex 
		critical: [anOrderedCollection addLast: Semaphore new].
	self exit.
	anIntegerOrNil ifNil: [
		lock wait
	] ifNotNil: [
		delay := MonitorDelay signalLock: lock afterMSecs: anIntegerOrNil inMonitor: self queue: anOrderedCollection.
		lock wait.
		delay unschedule.
	].
	self enter.
]

{ #category : #initialization }
Monitor >> initialize [
	super initialize.
	mutex := Semaphore forMutualExclusion.
	queuesMutex := Semaphore forMutualExclusion.
	nestingLevel := 0.
]

{ #category : #private }
Monitor >> isOwnerProcess [
	^ Processor activeProcess == ownerProcess
]

{ #category : #private }
Monitor >> privateCleanup [
	queuesMutex critical: [
		defaultQueue isEmpty ifTrue: [defaultQueue := nil].
		queueDict ifNotNil: [
			queueDict copy keysAndValuesDo: [:id :queue | 
				queue isEmpty ifTrue: [queueDict removeKey: id]].
			queueDict isEmpty ifTrue: [queueDict := nil].
		].
	].
]

{ #category : #private }
Monitor >> queueDict [
	^ queueDict ifNil: [queueDict := IdentityDictionary new].
	
]

{ #category : #private }
Monitor >> queueFor: aSymbol [ 
	aSymbol ifNil: [^self defaultQueue].
	^self queueDict at: aSymbol ifAbsentPut: [OrderedCollection new]
]

{ #category : #signaling-default }
Monitor >> signal [
	"One process waiting for the default event is woken up."

	^ self signal: nil
]

{ #category : #signaling-specific }
Monitor >> signal: aSymbolOrNil [
	"One process waiting for the given event is woken up. If there is no process waiting 
	for this specific event, a process waiting for the default event gets resumed."

	| queue |
	self checkOwnerProcess.
	queue := self queueFor: aSymbolOrNil.
	queue isEmpty ifTrue: [queue := self defaultQueue].
	self signalQueue: queue.
]

{ #category : #signaling-default }
Monitor >> signalAll [
	"All processes waiting for the default event are woken up."

	^ self signalAll: nil
]

{ #category : #signaling-specific }
Monitor >> signalAll: aSymbolOrNil [
	"All process waiting for the given event or the default event are woken up."

	| queue |
	self checkOwnerProcess.
	queue := self queueFor: aSymbolOrNil.
	self signalAllInQueue: self defaultQueue.
	queue ~~ self defaultQueue ifTrue: [self signalAllInQueue: queue].
]

{ #category : #private }
Monitor >> signalAllInQueue: anOrderedCollection [
	 queuesMutex critical: [
		anOrderedCollection removeAllSuchThat: [ :each |
				each signal.
				true ] ]
]

{ #category : #private }
Monitor >> signalLock: aSemaphore inQueue: anOrderedCollection [
	queuesMutex critical: [
		aSemaphore signal.
		anOrderedCollection remove: aSemaphore ifAbsent: [].
	].
]

{ #category : #private }
Monitor >> signalQueue: anOrderedCollection [
	queuesMutex critical: [
	 anOrderedCollection isEmpty ifFalse: [
			anOrderedCollection removeFirst signal ] ]
]

{ #category : #signaling-specific }
Monitor >> signalReallyAll [
	"All processes waiting for any events (default or specific) are woken up."

	self checkOwnerProcess.
	self signalAll.
	self queueDict valuesDo: [:queue |
		self signalAllInQueue: queue].
]

{ #category : #waiting-basic }
Monitor >> wait [
	"Unconditional waiting for the default event.
	The current process gets blocked and leaves the monitor, which means that the monitor
	allows another process to execute critical code. When the default event is signaled, the
	original process is resumed."

	^ self waitMaxMilliseconds: nil
]

{ #category : #waiting-specific }
Monitor >> waitFor: aSymbolOrNil [
	"Unconditional waiting for the non-default event represented by the argument symbol.
	Same as Monitor>>wait, but the process gets only reactivated by the specific event and 
	not the default event."

	^ self waitFor: aSymbolOrNil maxMilliseconds: nil
]

{ #category : #waiting-timeout }
Monitor >> waitFor: aSymbolOrNil maxMilliseconds: anIntegerOrNil [
	"Same as Monitor>>waitFor:, but the process gets automatically woken up when the 
	specified time has passed."

	self checkOwnerProcess.
	self waitInQueue: (self queueFor: aSymbolOrNil) maxMilliseconds: anIntegerOrNil.
]

{ #category : #waiting-timeout }
Monitor >> waitFor: aSymbolOrNil maxSeconds: aNumber [
	"Same as Monitor>>waitFor:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitFor: aSymbolOrNil maxMilliseconds: (aNumber * 1000) asInteger
]

{ #category : #private }
Monitor >> waitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil [
	self exitAndWaitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil.
]

{ #category : #waiting-timeout }
Monitor >> waitMaxMilliseconds: anIntegerOrNil [
	"Same as Monitor>>wait, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitFor: nil maxMilliseconds: anIntegerOrNil
]

{ #category : #waiting-timeout }
Monitor >> waitMaxSeconds: aNumber [
	"Same as Monitor>>wait, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitMaxMilliseconds: (aNumber * 1000) asInteger
]

{ #category : #waiting-basic }
Monitor >> waitUntil: aBlock [
	"Conditional waiting for the default event.
	See Monitor>>waitWhile: aBlock."

	^ self waitUntil: aBlock for: nil
]

{ #category : #waiting-specific }
Monitor >> waitUntil: aBlock for: aSymbolOrNil [
	"Confitional waiting for the non-default event represented by the argument symbol.
	See Monitor>>waitWhile:for: aBlock."

	^ self waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: nil
]

{ #category : #waiting-timeout }
Monitor >> waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: anIntegerOrNil [
	"Same as Monitor>>waitUntil:for:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: [aBlock value not] for: aSymbolOrNil maxMilliseconds: anIntegerOrNil
]

{ #category : #waiting-timeout }
Monitor >> waitUntil: aBlock for: aSymbolOrNil maxSeconds: aNumber [
	"Same as Monitor>>waitUntil:for:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: (aNumber * 1000) asInteger
]

{ #category : #waiting-timeout }
Monitor >> waitUntil: aBlock maxMilliseconds: anIntegerOrNil [
	"Same as Monitor>>waitUntil:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitUntil: aBlock for: nil maxMilliseconds: anIntegerOrNil
]

{ #category : #waiting-timeout }
Monitor >> waitUntil: aBlock maxSeconds: aNumber [
	"Same as Monitor>>waitUntil:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitUntil: aBlock maxMilliseconds: (aNumber * 1000) asInteger
]

{ #category : #waiting-basic }
Monitor >> waitWhile: aBlock [
	"Conditional waiting for the default event.
	The current process gets blocked and leaves the monitor only if the argument block
	evaluates to true. This means that another process can enter the monitor. When the 
	default event is signaled, the original process is resumed, which means that the condition
	(argument block) is checked again. Only if it evaluates to false, does execution proceed.
	Otherwise, the process gets blocked and leaves the monitor again..."

	^ self waitWhile: aBlock for: nil
]

{ #category : #waiting-specific }
Monitor >> waitWhile: aBlock for: aSymbolOrNil [
	"Confitional waiting for the non-default event represented by the argument symbol.
	Same as Monitor>>waitWhile:for:, but the process gets only reactivated by the specific 
	event and not the default event."

	^ self waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: nil
]

{ #category : #waiting-timeout }
Monitor >> waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: anIntegerOrNil [
	"Same as Monitor>>waitWhile:for:, but the process gets automatically woken up when the 
	specified time has passed."

	self checkOwnerProcess.
	self waitWhile: aBlock inQueue: (self queueFor: aSymbolOrNil) maxMilliseconds: anIntegerOrNil.
]

{ #category : #waiting-timeout }
Monitor >> waitWhile: aBlock for: aSymbolOrNil maxSeconds: aNumber [
	"Same as Monitor>>waitWhile:for:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: (aNumber * 1000) asInteger
]

{ #category : #private }
Monitor >> waitWhile: aBlock inQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil [
	[aBlock value] whileTrue: [self exitAndWaitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil].
]

{ #category : #waiting-timeout }
Monitor >> waitWhile: aBlock maxMilliseconds: anIntegerOrNil [
	"Same as Monitor>>waitWhile:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: aBlock for: nil maxMilliseconds: anIntegerOrNil
]

{ #category : #waiting-timeout }
Monitor >> waitWhile: aBlock maxSeconds: aNumber [
	"Same as Monitor>>waitWhile:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: aBlock maxMilliseconds: (aNumber * 1000) asInteger
]
