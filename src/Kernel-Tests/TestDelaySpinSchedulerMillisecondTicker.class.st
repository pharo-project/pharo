Class {
	#name : #TestDelaySpinSchedulerMillisecondTicker,
	#superclass : #TestDelaySpinSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelaySpinSchedulerMillisecondTicker >> classForTicker [
	^DelayMillisecondTickerSimulation 
]
