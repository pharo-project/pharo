"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelayBasicScheduler
  ticker DelayMillisecondTicker.


"
Class {
	#name : 'DelayBasicSchedulerMillisecondTickerTest',
	#superclass : 'DelayBasicSchedulerMicrosecondTickerTest',
	#category : 'Kernel-Tests-Delays',
	#package : 'Kernel-Tests',
	#tag : 'Delays'
}

{ #category : 'tests' }
DelayBasicSchedulerMillisecondTickerTest >> classForTicker [
	^ DelayMillisecondTickerSimulation
]
