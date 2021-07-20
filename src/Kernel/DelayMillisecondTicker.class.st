"
I interface to the millisecond based VM primitives.
"
Class {
	#name : #DelayMillisecondTicker,
	#superclass : #AbstractDelayTicker,
	#category : #'Kernel-Delays'
}

{ #category : #'api-system' }
DelayMillisecondTicker >> millisecondsUntilTick: microsecondsTick [
	^((microsecondsTick - self nowTick) max: 0)
]

{ #category : #'api-system' }
DelayMillisecondTicker >> nowTick [
	"Copied from Time class >> primMillisecondClock."
	"Primitive. Answer the number of milliseconds since the millisecond clock
	 was last reset or rolled over. Answer zero if the primitive fails."

	<primitive: 135>
	^ 0
]

{ #category : #'private - primitives' }
DelayMillisecondTicker >> primSignal: aSemaphore atMilliseconds: aSmallInteger [
	"Signal the semaphore when the millisecond clock reaches the value of the second argument. Fail if the first argument is neither a Semaphore nor nil. Essential. See Object documentation whatIsAPrimitive."
	<primitive: 136>
	^self primitiveFailed
	

]

{ #category : #'api-system' }
DelayMillisecondTicker >> tickAfterMilliseconds: milliseconds [
	^self nowTick "milliseconds" + milliseconds
]

{ #category : #'api-system' }
DelayMillisecondTicker >> waitForUserSignalled: timingSemaphore orExpired: activeDelay [
	|nextTick|
	"Sleep until the active delay is due, or timingSemaphore signalled by DelayScheduler user-api."
	
	"We sleep at most 1sec here as a soft busy-loop so that we don't accidentally miss signals."
	nextTick := self nowTick + (1"sec" * 1000"msecs").
	activeDelay ifNotNil: [
		nextTick := nextTick min: activeDelay resumptionTick ].
		
	timingSemaphore initSignals.
	self primSignal: timingSemaphore atMilliseconds: nextTick.
	"WARNING! Stepping <Over> the following line may lock the Image. Use <Proceed>."
	timingSemaphore wait.
	
]
