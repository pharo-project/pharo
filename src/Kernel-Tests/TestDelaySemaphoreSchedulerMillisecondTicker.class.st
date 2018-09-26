"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelaySemaphoreScheduler
  ticker DelayMillisecondTicker.


"
Class {
	#name : #TestDelaySemaphoreSchedulerMillisecondTicker,
	#superclass : #TestDelaySemaphoreSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelaySemaphoreSchedulerMillisecondTicker >> classForTicker [
	^DelayMillisecondTickerSimulation 
]
