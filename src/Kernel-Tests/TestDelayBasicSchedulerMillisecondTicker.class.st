Class {
	#name : #TestDelayBasicSchedulerMillisecondTicker,
	#superclass : #TestDelayBasicSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelayBasicSchedulerMillisecondTicker >> classForTicker [
	^ DelayMillisecondTickerSimulation
]
