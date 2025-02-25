"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelayMutexScheduler
  ticker DelayMillisecondTicker.


"
Class {
	#name : 'DelayMutexSchedulerMillisecondTickerTest',
	#superclass : 'DelayMutexSchedulerMicrosecondTickerTest',
	#category : 'Kernel-Tests-Delays',
	#package : 'Kernel-Tests',
	#tag : 'Delays'
}

{ #category : 'tests' }
DelayMutexSchedulerMillisecondTickerTest >> classForTicker [
	^DelayMillisecondTickerSimulation
]
