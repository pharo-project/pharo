"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelaySemaphoreScheduler
  ticker DelayMicrosecondTicker.


"
Class {
	#name : #DelaySemaphoreSchedulerMicrosecondTickerTest,
	#superclass : #DelayBasicSchedulerMicrosecondTickerTest,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
DelaySemaphoreSchedulerMicrosecondTickerTest >> classForScheduler [ 
	^ DelaySemaphoreScheduler
]
