"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelaySemaphoreScheduler
  ticker DelayMicrosecondTicker.


"
Class {
	#name : #TestDelaySemaphoreSchedulerMicrosecondTicker,
	#superclass : #TestDelayBasicSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelaySemaphoreSchedulerMicrosecondTicker >> classForScheduler [ 
	^ DelaySemaphoreScheduler
]
