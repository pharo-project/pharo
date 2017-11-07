"
This is a specialization of the class Delay that is used for the implementation of the class Monitor.
"
Class {
	#name : #MonitorDelay,
	#superclass : #Delay,
	#instVars : [
		'monitor',
		'queue'
	],
	#category : #Kernel-Processes
}

{ #category : #'instance creation' }
MonitorDelay class >> signalLock: aSemaphore afterMSecs: anInteger inMonitor: aMonitor queue: anOrderedCollection [
	anInteger < 0 ifTrue: [self error: 'delay times cannot be negative'].
	^ (self new setDelay: anInteger forSemaphore: aSemaphore monitor: aMonitor queue: anOrderedCollection) schedule; yourself
]

{ #category : #private }
MonitorDelay >> schedulerSignalWaitingProcess [
	"The delay time has elapsed; signal the waiting process."

	beingWaitedOn := false.
	monitor signalLock: delaySemaphore inQueue: queue.
]

{ #category : #private }
MonitorDelay >> setDelay: anInteger forSemaphore: aSemaphore monitor: aMonitor queue: anOrderedCollection [
	monitor := aMonitor.
	queue := anOrderedCollection.
	self setDelay: anInteger forSemaphore: aSemaphore.
]
