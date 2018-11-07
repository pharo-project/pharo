Class {
	#name : #TestDelaySpinSchedulerMicrosecondTicker,
	#superclass : #TestDelayBasicSchedulerMicrosecondTicker,
	#category : #'Kernel-Tests-Delays'
}

{ #category : #tests }
TestDelaySpinSchedulerMicrosecondTicker >> classForScheduler [
	^ DelaySpinScheduler
]
