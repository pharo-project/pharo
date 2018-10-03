"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelayMutexScheduler
  ticker DelayMillisecondTicker.


"
Class {
	#name : #TestDelayMutexSchedulerMillisecondTicker,
	#superclass : #TestDelayMutexSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelayMutexSchedulerMillisecondTicker >> classForTicker [
	^DelayMillisecondTickerSimulation 
]
