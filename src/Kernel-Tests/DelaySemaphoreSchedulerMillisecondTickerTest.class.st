"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelaySemaphoreScheduler
  ticker DelayMillisecondTicker.


"
Class {
	#name : #DelaySemaphoreSchedulerMillisecondTickerTest,
	#superclass : #DelaySemaphoreSchedulerMicrosecondTickerTest,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
DelaySemaphoreSchedulerMillisecondTickerTest >> classForTicker [
	^DelayMillisecondTickerSimulation 
]
