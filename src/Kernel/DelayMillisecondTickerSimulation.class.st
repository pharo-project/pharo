"
To facilitate unit tests without interferring with the live VM interface,
I simulate the VM code for the millisecond primitives.
"
Class {
	#name : #DelayMillisecondTickerSimulation,
	#superclass : #DelayMillisecondTicker,
	#instVars : [
		'vmSimNow',
		'vmSimTheTimerSemaphore',
		'vmSimNextWakeupTick'
	],
	#category : #'Kernel-Delays'
}

{ #category : #initialization }
DelayMillisecondTickerSimulation >> initialize [
	super initialize.
	vmSimTheTimerSemaphore := Semaphore new.
	vmSimNow := 0.

]

{ #category : #'api-system' }
DelayMillisecondTickerSimulation >> nowTick [
	^vmSimNow
]

{ #category : #'private - primitives' }
DelayMillisecondTickerSimulation >> primSignal: sempahore atMilliseconds: nextTick [
	"This is a simulation of the primitive, working hand-in-hand with #simulate_vmMilliseconds:"
	(sempahore isKindOf: Semaphore) ifTrue: [ 
		vmSimTheTimerSemaphore := sempahore.
		vmSimNextWakeupTick := nextTick ].
	(sempahore = nil) ifTrue: [ 
		vmSimTheTimerSemaphore := nil.
		vmSimNextWakeupTick := 0] 

		
]

{ #category : #'api-system' }
DelayMillisecondTickerSimulation >> simulate_vmMilliseconds: milliseconds [ 
	
	vmSimNextWakeupTick ~= 0 ifTrue: [ 
		vmSimNow := milliseconds.
		vmSimNow >= vmSimNextWakeupTick ifTrue: [ 
			vmSimNextWakeupTick := 0. 
			vmSimTheTimerSemaphore signal.
			].
 		].
	



]

{ #category : #'api-system' }
DelayMillisecondTickerSimulation >> vmSimNextWakeupMilliseconds [
	^vmSimNextWakeupTick
]
