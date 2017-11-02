"
I test EpMonitor integration (with change recording actually enabled).
"
Class {
	#name : #EpEnabledIntegrationTest,
	#superclass : #EpMonitorIntegrationTest,
	#category : 'EpiceaTests-Integration'
}

{ #category : #running }
EpEnabledIntegrationTest >> setUp [
	super setUp.
	monitor enable.
]
