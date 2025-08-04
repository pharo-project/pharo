Class {
	#name : 'SignalLoggerTest',
	#superclass : 'TestCase',
	#category : 'Beacon-Core-Tests-Tests',
	#package : 'Beacon-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SignalLoggerTest >> testRunForFiltersSignals [

	| logger |
	logger := MemoryLogger new.
	logger runFor: StringSignal during: [
			StringSignal emit: 'This should be recorded.'.
			DummySignal new emit.
			StringSignal emit: 'This one too' ].
	self assert: logger entries size equals: 2
]

{ #category : 'tests' }
SignalLoggerTest >> testRunForMultipleSignals [

	| logger |
	logger := MemoryLogger new.
	logger runFor: StringSignal, DummySignal during: [
			StringSignal emit: 'This should be recorded.'.
			DummySignal new emit.
			StringSignal emit: 'This one too' ].
	self assert: logger entries size equals: 3
]

{ #category : 'tests' }
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
