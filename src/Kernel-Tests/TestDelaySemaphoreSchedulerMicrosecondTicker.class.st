Class {
	#name : #TestDelaySemaphoreSchedulerMicrosecondTicker,
	#superclass : #TestDelayBasicSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelaySemaphoreSchedulerMicrosecondTicker >> classForScheduler [ 
	^ DelaySemaphoreScheduler
]
