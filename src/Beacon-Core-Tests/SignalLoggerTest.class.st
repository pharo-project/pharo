"
A SignalLoggerTest is xxxxxxxxx.

"
Class {
	#name : #SignalLoggerTest,
	#superclass : #TestCase,
	#category : #'Beacon-Core-Tests-Tests'
}

{ #category : #tests }
SignalLoggerTest >> testStartingTwiceDoesNotRegisterTwice [
	| initialNumberOfSubscriptions |
	initialNumberOfSubscriptions := Beacon instance announcer numberOfSubscriptions.
	SignalLogger resetInstance.
	SignalLogger start.
	SignalLogger start.
	self 
		assert: Beacon instance announcer numberOfSubscriptions 
		equals: initialNumberOfSubscriptions + 1.
	SignalLogger stop.
	self 
		assert: Beacon instance announcer numberOfSubscriptions 
		equals: initialNumberOfSubscriptions
]
