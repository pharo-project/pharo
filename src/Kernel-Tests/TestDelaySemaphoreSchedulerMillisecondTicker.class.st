Class {
	#name : #TestDelaySemaphoreSchedulerMillisecondTicker,
	#superclass : #TestDelaySemaphoreSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelaySemaphoreSchedulerMillisecondTicker >> classForTicker [
	^DelayMillisecondTickerSimulation 
]
