Class {
	#name : #TestDelayMutexSchedulerMillisecondTicker,
	#superclass : #TestDelayMutexSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelayMutexSchedulerMillisecondTicker >> classForTicker [
	^DelayMillisecondTickerSimulation 
]
