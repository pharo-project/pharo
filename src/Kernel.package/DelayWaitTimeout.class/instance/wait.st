waiting
wait
	"Wait until either the semaphore is signaled or the delay times out"
	[self schedule.
	"It is critical that the following has no suspension point so that
	the test and the wait primitive are atomic. In addition, if the delay
	is no longer being waited on while entering the way we know that it 
	is expired because the delay has already fired."
	beingWaitedOn 
		ifTrue:[delaySemaphore wait]
		ifFalse:[expired := true]] ensure:[self unschedule].
	^self isExpired
