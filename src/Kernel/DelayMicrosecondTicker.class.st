"
I interface to the microsecond based VM primitives.
"
Class {
	#name : #DelayMicrosecondTicker,
	#superclass : #AbstractDelayTicker,
	#category : #'Kernel-Delays'
}

{ #category : #'api-system' }
DelayMicrosecondTicker >> millisecondsUntilTick: microsecondsTick [
	^((microsecondsTick - self nowTick) max: 0) / 1000 
]

{ #category : #'api-system' }
DelayMicrosecondTicker >> nowTick [
	 "Copied from Time class >> primUTCMicrosecondsClock.
	 Answer the number of micro-seconds ellapsed since Squeak epoch.
	 That is since 00:00 on the morning of January 1, 1901 UTC.
	 At least a 60-bit unsigned integer is used internally which is enough for dates up to year 38435.
	 Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 240>
	self primitiveFailed
]

{ #category : #'private-primitives' }
DelayMicrosecondTicker >> primSignal: aSemaphore atUTCMicroseconds: aLargePositiveInteger [
	"Signal the semaphore when the microsecond clock reaches the value of the second argument. Fail if the first argument is neither a Semaphore nor nil. Essential. See Object documentation whatIsAPrimitive."
	<primitive: 242>
	^ self primitiveFailed
	
"primitiveSignalAtUTCMicroseconds
	Cause the time semaphore, if one has been registered, to be
	 signalled when the microsecond clock is greater than or equal to
	 the given tick value. A tick value of zero turns off timer interrupts.
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
	self primitiveFailFor: PrimErrBadArgument"
]

{ #category : #'api-system' }
DelayMicrosecondTicker >> tickAfterMilliseconds: milliseconds [
	^self nowTick "microseconds" + (1000 * milliseconds)
]

{ #category : #'api-system' }
DelayMicrosecondTicker >> waitForUserSignalled: timingSemaphore orExpired: activeDelay [
	|nextTick|
	"Sleep until the active delay is due, or timingSemaphore signalled by DelayScheduler user-api."
	
	"We sleep at most 1sec here as a soft busy-loop so that we don't accidentally miss signals."
	nextTick := self nowTick + (1"sec" * 1000"msecs" * 1000"usecs").
	activeDelay ifNotNil: [
		nextTick := nextTick min: activeDelay resumptionTick ].
		
	timingSemaphore initSignals.
	self primSignal: timingSemaphore atUTCMicroseconds: nextTick.
	"WARNING! Stepping <Over> the following line may lock the Image. Use <Proceed>."
	timingSemaphore wait.
	
]
