"
I inherit delay scheduler system tests 
and run them for the specific combination of...
  scheduler DelayBasicScheduler
  ticker DelayMillisecondTicker.


"
Class {
	#name : #TestDelayBasicSchedulerMillisecondTicker,
	#superclass : #TestDelayBasicSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelayBasicSchedulerMillisecondTicker >> classForTicker [
	^ DelayMillisecondTickerSimulation
]
