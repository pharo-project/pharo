"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelayMutexScheduler
  ticker DelayMicrosecondTicker.


"
Class {
	#name : #TestDelayMutexSchedulerMicrosecondTicker,
	#superclass : #TestDelayBasicSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelayMutexSchedulerMicrosecondTicker >> classForScheduler [ 
	^ DelayMutexScheduler
]
