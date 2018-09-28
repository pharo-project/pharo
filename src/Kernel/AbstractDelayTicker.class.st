"
I represent the interface to the VM time-base primitives for tick generation and signalling.
This facilitates the DelaySchedulerXXX implementations to be independent of any particular time-based. 
I supply /nextTick/ to the VM and wait for /timingSemaphore/ to be signalled.

I read time-base specific ticks from the VM, and scale ticks around snapshot pauses.
"
Class {
	#name : #AbstractDelayTicker,
	#superclass : #Object,
	#category : #'Kernel-Delays'
}

{ #category : #'api-system' }
AbstractDelayTicker >> millisecondsUntilTick: delay [
	self subclassResponsibility 
]

{ #category : #'api-system' }
AbstractDelayTicker >> nowTick [
	self subclassResponsibility 
]

{ #category : #'api-system' }
AbstractDelayTicker >> restoreResumptionTimes: delaysOrNils [
	"Private! Called only from the timing-priority process.
    Not performance critical."

	|newBaseTick|
		
	"Restore resumption ticks of delays relative to a base tick of zero."
	newBaseTick := self nowTick.
	delaysOrNils do: [ :delay |
		delay ifNotNil: [delay resumptionTickAdjustFrom: 0 to: newBaseTick ]].
]

{ #category : #'api-system' }
AbstractDelayTicker >> saveResumptionTimes: delaysOrNils [
	"Private! Called only from the timing-priority process.
    Not performance critical."

	|oldBaseTick|
	
	"Record the resumption ticks of delays relative to a base tick of zero."
	oldBaseTick := self nowTick.
	delaysOrNils do: [ :delay |
		delay ifNotNil: [delay resumptionTickAdjustFrom: oldBaseTick to: 0 ]].

]

{ #category : #'api-system' }
AbstractDelayTicker >> tickAfterMilliseconds: milliseconds [
	self subclassResponsibility 
]

{ #category : #'api-system' }
AbstractDelayTicker >> waitForUserSignalled: timingSemaphore orExpired: activeDelay [
	self subclassResponsibility
]
