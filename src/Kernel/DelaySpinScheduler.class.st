"
I'm just like DelayBasicScheduler except I busy-wait for the backend to clear the transfer variable.
In a single-native-thread environment I provide a bit more assurance than the basic scheduler,
but I remain dependent on implict scheduling semantics such that no suspension point follows 
inlined #whileFalse: or assignments, which may not hold true for the threaded-vm.

IMPORTANT! If #schedule: is called twice by a thread running at #highestPriority 
the system will freeze since the busy loop blocks #scheduleAtTimingPriority 
from being able to clear the transfer variable. i.e. this delay scheduler assumes
#runBackendLoopAtTimingPriority is the only process running at #highestPriority.

Same deal for #unschedule:
"
Class {
	#name : #DelaySpinScheduler,
	#superclass : #DelayBasicScheduler,
	#category : #'Kernel-Delays'
}

{ #category : #'low priority processes' }
DelaySpinScheduler >> schedule: aDelay [
	"Depends on implicit semantics of bytecode inlining and cooperative multitasking 
	 such that no suspension can occur between whileFalse, assignment & signal.
	 i.e. there are no backward jumps after comparison becomes true."
		
	aDelay schedulerBeingWaitedOn ifTrue: [^self error: 'This Delay has already been scheduled.'].
	[ delayToStart == nil ] whileFalse. 
	delayToStart := aDelay. 
	timingSemaphore signal. "Transfer execution to back-end #scheduleAtTimingPriority"

]

{ #category : #'low priority processes' }
DelaySpinScheduler >> unschedule: aDelay [
	"Depends on implicit semantics of bytecode inlining and cooperative multitasking 
	 such that no suspension can occur between whileFalse, assignment & signal.
	 i.e. there are no backward jumps after comparison becomes true."
	
	[delayToStop == nil] whileFalse.
	delayToStop := aDelay. 
	timingSemaphore signal. "Transfer execution to back-end #scheduleAtTimingPriority"
]
