"
I add shared-resource based synchronization to the basic scheduler.
"
Class {
	#name : #DelayMutexScheduler,
	#superclass : #DelayBasicScheduler,
	#instVars : [
		'accessProtect'
	],
	#category : #'Kernel-Delays'
}

{ #category : #initialization }
DelayMutexScheduler >> initializeTicker: aDelayTicker suspendedDelaysHeap: aHeap [
	super initializeTicker: aDelayTicker suspendedDelaysHeap: aHeap.
	accessProtect := Semaphore forMutualExclusion.

]

{ #category : #initialization }
DelayMutexScheduler >> schedule: aDelay [
	accessProtect critical: [ super schedule: aDelay].
	
]

{ #category : #initialization }
DelayMutexScheduler >> unschedule: aDelay [
	accessProtect critical: [ super unschedule: aDelay].
	
]
