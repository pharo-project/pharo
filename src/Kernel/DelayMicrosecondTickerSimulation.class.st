"
To facilitate unit tests without interferring with the live VM interface,
I simulate the VM code for the microsecond primitives.
"
Class {
	#name : #DelayMicrosecondTickerSimulation,
	#superclass : #DelayMicrosecondTicker,
	#instVars : [
		'vmSimNow',
		'vmSimTheTimerSemaphore',
		'vmSimNextWakeupTick'
	],
	#category : #'Kernel-Delays'
}

{ #category : #initialization }
DelayMicrosecondTickerSimulation >> initialize [
	super initialize.
	vmSimTheTimerSemaphore := Semaphore new.
	vmSimNow := 0.

]

{ #category : #'api-system' }
DelayMicrosecondTickerSimulation >> nowTick [
	^vmSimNow
]

{ #category : #'private - primitives' }
DelayMicrosecondTickerSimulation >> primSignal: sempahore atUTCMicroseconds: nextTick [ 
	"This is a simulation of the primitive, working hand-in-hand with #simulate_vmMilliseconds:"
	(sempahore isKindOf: Semaphore) ifTrue: [ 
		vmSimTheTimerSemaphore := sempahore.
		vmSimNextWakeupTick := nextTick ].
	(sempahore = nil) ifTrue: [ 
		vmSimTheTimerSemaphore := nil.
		vmSimNextWakeupTick := 0] 
	
"Extract from VM code...
StackInterpreterPrimitives >> primitiveSignalAtUTCMicroseconds
	...Cause the time semaphore, if one has been registered, to be
	 signalled when the microsecond clock is greater than or equal to
	 the given tick value. A tick value of zero turns off timer interrupts...
	| usecsObj sema usecs |
	<var: #usecs type: #usqLong>
	usecsObj := self stackTop.
	sema := self stackValue: 1.
	usecs := self positive64BitValueOf: usecsObj.
	self successful ifTrue:
		[(objectMemory isSemaphoreOop: sema) ifTrue:
			[objectMemory splObj: TheTimerSemaphore put: sema.
			 nextWakeupUsecs := usecs.
			 ^self pop: 2].
		 sema = objectMemory nilObject ifTrue:
			[objectMemory
				storePointer: TheTimerSemaphore
				ofObject: objectMemory specialObjectsOop
				withValue: objectMemory nilObject.
			 nextWakeupUsecs := 0.
			 ^self pop: 2]].
	self primitiveFailFor: PrimErrBadArgument
"	
		
]

{ #category : #'api-system' }
DelayMicrosecondTickerSimulation >> simulate_vmMilliseconds: milliseconds [ 
	
	vmSimNextWakeupTick ~= 0 ifTrue: [ 
		vmSimNow := milliseconds * 1000.
		vmSimNow >= vmSimNextWakeupTick ifTrue: [ 
			vmSimNextWakeupTick := 0. 
			vmSimTheTimerSemaphore signal.
			].
 		].

"Extract from VM code... 
StackInterpreter>>checkForEventsMayContextSwitch:
	nextWakeupUsecs ~= 0 ifTrue:
		[now >= nextWakeupUsecs ifTrue:
			[nextWakeupUsecs := 0.
			 ...set timer interrupt to 0 for 'no timer'...
			 sema := objectMemory splObj: TheTimerSemaphore.
			 (sema ~= objectMemory nilObject
			  and: [self synchronousSignal: sema]) ifTrue:
				[switched := true]]].
"
	



]

{ #category : #'api-system' }
DelayMicrosecondTickerSimulation >> vmSimNextWakeupMilliseconds [
	^vmSimNextWakeupTick / 1000
]
