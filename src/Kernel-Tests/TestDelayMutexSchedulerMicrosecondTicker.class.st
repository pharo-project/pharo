Class {
	#name : #TestDelayMutexSchedulerMicrosecondTicker,
	#superclass : #TestDelayBasicSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelayMutexSchedulerMicrosecondTicker >> classForScheduler [ 
	^ DelayMutexScheduler
]
