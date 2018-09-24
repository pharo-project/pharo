"
In contrast to the traditional ticker (DelayMicrosecondTicker) 
I am not capped at a maximum sleep of 1 second, making a quieter idle image.
When there is no scheduled delay, the VM will sleep indefinitely until woken 
by another mechanism (which is what?)
"
Class {
	#name : #DelayMicrosecondUncappedTicker,
	#superclass : #DelayMicrosecondTicker,
	#category : #'Kernel-Delays'
}

{ #category : #'api-system' }
DelayMicrosecondUncappedTicker >> waitForUserSignalled: timingSemaphore orExpired: activeDelay [
	|nextTick|
	"Sleep until the active delay is due, or timingSemaphore signalled by DelayScheduler user-api."
	
	"Signal when the next request is due. We trust our VM and will sleep as long as needed. 
	 If there is no activeDelay, nextTick = 0 will turn off the timer interrupt."
	nextTick := 0.
	activeDelay ifNotNil: [
		nextTick := nextTick min: activeDelay resumptionTick ].
		
	timingSemaphore initSignals.
	self primSignal: timingSemaphore atUTCMicroseconds: nextTick.
	"WARNING! Stepping <Over> the following line may lock the Image. Use <Proceed>."
	timingSemaphore wait.
	
]
