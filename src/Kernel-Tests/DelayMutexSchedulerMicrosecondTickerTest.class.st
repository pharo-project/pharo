"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelayMutexScheduler
  ticker DelayMicrosecondTicker.


"
Class {
	#name : #DelayMutexSchedulerMicrosecondTickerTest,
	#superclass : #DelayBasicSchedulerMicrosecondTickerTest,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
DelayMutexSchedulerMicrosecondTickerTest >> classForScheduler [ 
	^ DelayMutexScheduler
]
